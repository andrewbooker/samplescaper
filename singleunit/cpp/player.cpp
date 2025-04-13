
#include "interleave.h"

#include <iostream>
#include <portaudio.h>
#include <sndfile.h>
#include <string>
#include <cstring>


class AudioPlayer {
private:
    SNDFILE* audioFileL;
    SNDFILE* audioFileR;
    PaStream* audioStream;
    const unsigned int channels;

    void readInto(float* out, const unsigned long perChannelLength) {
        memset(out, 0, perChannelLength * sizeof(float) * channels);

        sf_readf_float(audioFileL, out, perChannelLength);
        sf_readf_float(audioFileR, out + perChannelLength, perChannelLength);
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
    AudioPlayer(const std::string& filePath): channels(2), audioFileL(0), audioFileR(0), audioStream(0) {
        if (Pa_Initialize() != paNoError) {
            std::cerr << "PortAudio initialization failed." << std::endl;
            return;
        }

        const std::string left(filePath + "/looped_65_si_2025-02-14_220120.wav");
        const std::string right(filePath + "/looped_69_si_2025-02-14_220055.wav");

        SF_INFO ignore;
        audioFileL = sf_open(left.c_str(), SFM_READ, &ignore);
        audioFileR = sf_open(right.c_str(), SFM_READ, &ignore);

        PaStreamParameters outputParameters;
        memset(&outputParameters, 0, sizeof(PaStreamParameters));
        outputParameters.device = 0;
        outputParameters.channelCount = channels;
        outputParameters.sampleFormat = paFloat32;
        outputParameters.suggestedLatency = Pa_GetDeviceInfo(outputParameters.device)->defaultLowOutputLatency;

        if (Pa_OpenStream(&audioStream, 0, &outputParameters, 44100, paFramesPerBufferUnspecified, paClipOff, &AudioPlayer::audioCallback, this) != paNoError) {
            std::cerr << "Failed to open PortAudio stream." << std::endl;
        }
    }
    
    ~AudioPlayer() {
        if (audioFileL) {
            sf_close(audioFileL);
        }
        if (audioFileR) {
            sf_close(audioFileR);
        }

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
        if (!audioFileL) {
            std::cout << "No audio file" << std::endl;
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
