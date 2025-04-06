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

    void readInto(float* out, const unsigned long upToLength) {
        memset(out, 0, upToLength * sizeof(float));

        const unsigned long perFileLength(upToLength / channels);

        sf_readf_float(audioFileL, out + 0, perFileLength);
        sf_readf_float(audioFileR, out + perFileLength, perFileLength);
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

        const std::string left(filePath + "/looped_55_si_2025-02-23_154447.wav");
        const std::string right(filePath + "/looped_86_siq_2025-02-23_105403.wav");

        SF_INFO ignore;
        audioFileL = sf_open(left.c_str(), SFM_READ, &ignore);
        audioFileR = sf_open(right.c_str(), SFM_READ, &ignore);

        PaStreamParameters outputParameters;
        outputParameters.device = Pa_GetDefaultOutputDevice();
        outputParameters.channelCount = channels;
        outputParameters.sampleFormat = paFloat32;
        outputParameters.suggestedLatency = Pa_GetDeviceInfo(outputParameters.device)->defaultLowOutputLatency;
        outputParameters.hostApiSpecificStreamInfo = 0;

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
        if (audioFileL && audioStream) {
            if (Pa_StartStream(audioStream) == paNoError) {
                std::cout << "Playing audio. Press Enter to stop..." << std::endl;
                std::cin.get();
                return true;
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



int main() {
    const std::string filePath("/home/abooker/Music/pool/looped");

    AudioPlayer audioPlayer(filePath);

    if (audioPlayer.start()) {
        audioPlayer.stop();
    }
   
    return 0;
}
