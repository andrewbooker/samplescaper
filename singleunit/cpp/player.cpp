#include "interleave.h"
#include "httpSource.h"
#include "soundSource.h"

#include <iostream>
#include <portaudio.h>
#include <sndfile.h>
#include <string>
#include <cstring>
#include <vector>
#include <filesystem>
#include <time.h>
#include <chrono>

class DiskSource : public SoundSource {
private:
    const std::string& location;
    typedef std::vector<std::string> t_fileNames;
    t_fileNames fileNames;

protected:
    SNDFILE* getSoundFile() {
        for (const auto & entry : std::filesystem::directory_iterator(location)) {
            fileNames.push_back(entry.path());
        }
        const unsigned int selection(rand() % fileNames.size());
        SF_INFO info;
        return sf_open(fileNames[selection].c_str(), SFM_READ, &info);
    }

public:
    DiskSource(const std::string& loc) : SoundSource(), location(loc) {
    }
};


class SoundSources {
private:
    typedef std::vector<SoundSource*> t_sources;
    t_sources sources;

public:
    SoundSources(const std::string& filePath, const unsigned int channels) {
        for (unsigned int c(0); c != channels; ++c) {
            //sources.push_back(new DiskSource(filePath));
            sources.push_back(new HttpSource(c));
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
    AudioPlayer(const std::string& filePath) :
        channels(2),
        audioStream(0),
        soundSources(filePath, 2)
    {
        if (Pa_Initialize() != paNoError) {
            std::cerr << "PortAudio initialization failed." << std::endl;
            return;
        }

        PaStreamParameters outputParameters;
        memset(&outputParameters, 0, sizeof(PaStreamParameters));
        outputParameters.device = Pa_GetDefaultOutputDevice();
        outputParameters.channelCount = channels;
        outputParameters.sampleFormat = paFloat32;
        outputParameters.suggestedLatency = Pa_GetDeviceInfo(outputParameters.device)->defaultLowOutputLatency;

        if (Pa_OpenStream(&audioStream, 0, &outputParameters, 44100, paFramesPerBufferUnspecified, paClipOff, &AudioPlayer::audioCallback, this) != paNoError) {
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



int main() {
    srand(time(0));
    const std::string filePath("/home/abooker/Music/pool/looped");

    AudioPlayer audioPlayer(filePath);

    if (audioPlayer.start()) {
        audioPlayer.stop();
    } else {
        std::cout << "could not start playback" << std::endl;
    }
   
    return 0;
}
