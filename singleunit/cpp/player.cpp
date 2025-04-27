#include "interleave.h"
#include "httpSoundSource.h"
#include "soundSource.h"

#include <iostream>
#include <portaudio.h>
#include <string>
#include <cstring>
#include <vector>


class SoundSources {
private:
    typedef std::vector<SoundSource*> t_sources;
    t_sources sources;

public:
    SoundSources(const unsigned int channels, const HttpSoundSource::t_hosts& hosts) {
        for (unsigned int c(0); c != channels; ++c) {
            sources.push_back(new HttpSoundSource(c, hosts));
        }
    }

    void readInto(float* out, const unsigned long sampleLength, const unsigned int channel) {
        sources[channel]->readInto(out, sampleLength);
    }

    ~SoundSources() {
        for (t_sources::const_iterator i(sources.begin()); i != sources.end(); ++i) {
            delete *i;
        }
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

public:
    AudioPlayer(const HttpSoundSource::t_hosts& hosts, const unsigned int ch, const unsigned int devNum) :
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
            std::cout << "Playing audio. Press Enter to stop..." << std::endl;
            std::cin.get();
            return true;
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
    HttpSoundSource::t_hosts hosts;
    const std::string server("0.0.0.0");
    const unsigned int channels(argc > 1 ? atoi(argv[1]) : 2);
    const unsigned int deviceNumber(argc > 2 ? atoi(argv[2]) : 0);
    std::cout << "playing " << channels << " channels on device " << deviceNumber << "\n";
    if (argc > 3) {
        for (int i(3); i != argc; ++i) {
            const std::string port(argv[i]);
            hosts.push_back(server + ":" + port);
        }
    }
    if (hosts.empty()) {
        std::cout << "No hosts specified. Using ";
        hosts.push_back("0.0.0.0:9964");
    }
    for (auto& h : hosts) {
        std::cout << h << std::endl;
    }
    AudioPlayer audioPlayer(hosts, channels, deviceNumber);
    if (audioPlayer.start()) {
        audioPlayer.stop();
    } else {
        std::cout << "could not start playback" << std::endl;
    }

    return 0;
}
