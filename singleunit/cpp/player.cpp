
#include "interleave.h"

#include <iostream>
#include <portaudio.h>
#include <sndfile.h>
#include <string>
#include <cstring>


class DiskSource {
private:
    SNDFILE* soundFile;
    SF_INFO info;

public:
    DiskSource(const std::string& f) : soundFile(sf_open(f.c_str(), SFM_READ, &info)) {
    }

    void readInto(float* out, const unsigned long sampleLength) {
        sf_readf_float(soundFile, out, sampleLength);
    }

    ~DiskSource() {
        if (soundFile) {
            sf_close(soundFile);
        }
    }
};

class AudioPlayer {
private:
    DiskSource audioFileL;
    DiskSource audioFileR;
    PaStream* audioStream;
    const unsigned int channels;

    void readInto(float* out, const unsigned long perChannelLength) {
        memset(out, 0, perChannelLength * sizeof(float) * channels);

        audioFileL.readInto(out, perChannelLength);
        audioFileR.readInto(out + perChannelLength, perChannelLength);
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
        audioFileL(filePath + "/looped_65_si_2025-02-14_220120.wav"),
        audioFileR(filePath + "/looped_69_si_2025-02-14_220055.wav")
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
    const std::string filePath("/home/abooker/Music/pool/looped");

    AudioPlayer audioPlayer(filePath);

    if (audioPlayer.start()) {
        audioPlayer.stop();
    } else {
        std::cout << "could not start playback" << std::endl;
    }
   
    return 0;
}
