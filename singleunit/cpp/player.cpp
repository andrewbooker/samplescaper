#include "interleave.h"
#include "httpSoundSource.h"
#include "soundSource.h"

#include <iostream>
#include <portaudio.h>
#include <string>
#include <cstring>
#include <vector>
#include <termios.h>
#include <stdio.h>
#include <unistd.h>


class SoundSources {
private:
    typedef std::vector<SoundSource*> t_sources;
    t_sources sources;
    enum State { playing, pausing, paused, stopped } state;

    const bool anyPlaying() const {
        for (const auto* s : sources) {
            if (s->isPlaying()) {
                return true;
            }
        }
        return false;
    }

    void stopPlaying() {
        state = State::pausing;
        for (auto* s : sources) s->setPaused(true);
        while (anyPlaying()) {
            std::this_thread::sleep_for(std::chrono::milliseconds(10));
        }
    }

public:
    SoundSources(const unsigned int channels, OptionsProvider& hosts) : state(State::stopped) {
        for (unsigned int c(0); c != channels; ++c) {
            sources.push_back(new HttpSoundSource(c, hosts));
        }
        state = State::playing;
    }

    void readInto(float* out, const unsigned long sampleLength, const unsigned int channel) {
        sources[channel]->readInto(out, sampleLength);
    }

    void pause() {
        if (state != State::playing) return;
        stopPlaying();
        state = State::paused;
        std::cout << "Paused" << std::endl;
    }

    void resume() {
        if (state != State::paused) return;
        for (auto* s : sources) s->setPaused(false);
        state = State::playing;
    }

    void stop() {
        stopPlaying();
        state = State::stopped;
    }

    ~SoundSources() {
        for (auto* s : sources) delete s;
    }
};


class AudioPlayer {
private:
    SoundSources soundSources;
    PaStream* audioStream;
    const unsigned int channels;
    const unsigned int deviceNumber;

    void readInto(float* out, const unsigned long perChannelLength) {
        memset(out, 0, perChannelLength * sizeof(float) * channels);
        for (unsigned int c(0); c != channels; ++c) {
            soundSources.readInto(out + (c * perChannelLength), perChannelLength, c);
        }
        interleave(out, perChannelLength * channels, channels);
    }

    static int audioCallback(
        const void* inputBuffer,
        void* outputBuffer,
        unsigned long framesPerBuffer,
        const PaStreamCallbackTimeInfo* timeInfo,
        PaStreamCallbackFlags statusFlags,
        void* player
    ) {
        float* out(reinterpret_cast<float*>(outputBuffer));
        reinterpret_cast<AudioPlayer*>(player)->readInto(out, framesPerBuffer);
        return paContinue;
    }

    static const char getch() {
        char c(0);
        struct termios old {0};
        fflush(stdout);
        if (tcgetattr(0, &old) < 0) {
            perror("tcgetattr");
	    }
        old.c_lflag &= ~ICANON;
        old.c_lflag &= ~ECHO;
        old.c_cc[VMIN] = 1;
        old.c_cc[VTIME] = 0;
        tcsetattr(0, TCSANOW, &old);
        read(0, &c, 1);
        old.c_lflag |= ICANON;
        old.c_lflag |= ECHO;
        tcsetattr(0, TCSADRAIN, &old);
	    return c;
    }

public:
    AudioPlayer(OptionsProvider& hosts, const unsigned int ch, const unsigned int devNum) :
        channels(ch),
        deviceNumber(devNum),
        audioStream(0),
        soundSources(ch, hosts)
    {
        if (Pa_Initialize() != paNoError) {
            std::cerr << "PortAudio initialization failed." << std::endl;
            return;
        }

        PaStreamParameters outputParameters;
        memset(&outputParameters, 0, sizeof(PaStreamParameters));
        outputParameters.device = deviceNumber;
        outputParameters.channelCount = channels;
        outputParameters.sampleFormat = paFloat32;
        outputParameters.suggestedLatency = Pa_GetDeviceInfo(outputParameters.device)->defaultLowOutputLatency;

        if (Pa_OpenStream(&audioStream, 0, &outputParameters, 44100, 1024, paClipOff, &AudioPlayer::audioCallback, this) != paNoError) {
            std::cerr << "Failed to open PortAudio stream." << std::endl;
        }
    }
    
    ~AudioPlayer() {
        if (audioStream) {
            Pa_StopStream(audioStream);
            Pa_CloseStream(audioStream);
            Pa_Terminate();
        }
    }

    bool start() {
        if (!audioStream) {
            std::cout << "No audio stream" << std::endl;
            return false;
        }
        if (Pa_StartStream(audioStream) == paNoError) {
            std::cout << "Playing audio. Press p to pause, r to resume or q to stop." << std::endl;
            while (true) {
	            const char input(getch());
                if (input == 'q') {
                    std::cout << "Stopping" << std::endl;
                    soundSources.stop();
                    return true;
                }
	            if (input == 'p') {
                    std::cout << "Pausing" << std::endl;
                    soundSources.pause();
                }
	            if (input == 'r') {
                    std::cout << "Resuming" << std::endl;
                    soundSources.resume();
                }
            }
        }
        return false;
    }
    
    bool stop() {
        if (audioStream) {
            return Pa_StopStream(audioStream) == paNoError;
        }
        return false;
    }
};



int main(int argc, char *argv[]) {
    srand(time(0));
    const unsigned int channels(argc > 1 ? atoi(argv[1]) : 2);
    const unsigned int deviceNumber(argc > 2 ? atoi(argv[2]) : 0);
    std::cout << "playing " << channels << " channels on device " << deviceNumber << "\n";
    std::vector<std::string> ports;
    if (argc > 3) {
        for (int i(3); i != argc; ++i) {
            ports.push_back(argv[i]);
        }
    }
    if (ports.empty()) {
        std::cout << "No ports specified. Using ";
        ports.push_back("9964");
    }
    OptionsProvider hosts;
    const std::string localhost("0.0.0.0");
    for (auto& p : ports) {
        std::cout << p << std::endl;
        hosts.add(p.find(':') == std::string::npos ? (localhost + ":" + p) : p);
    }

    AudioPlayer audioPlayer(hosts, channels, deviceNumber);
    if (audioPlayer.start()) {
        audioPlayer.stop();
    } else {
        std::cout << "could not start playback" << std::endl;
    }

    return 0;
}
