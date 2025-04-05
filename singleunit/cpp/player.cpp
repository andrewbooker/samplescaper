#include <iostream>
#include <portaudio.h>
#include <sndfile.h>
#include <string>


static int audioCallback(
    const void* inputBuffer,
    void* outputBuffer,
    unsigned long framesPerBuffer,
    const PaStreamCallbackTimeInfo* timeInfo,
    PaStreamCallbackFlags statusFlags,
    void* userData
) {
    SNDFILE* sndFile = (SNDFILE*)userData;
    float* out = (float*)outputBuffer;

    sf_count_t framesRead = sf_readf_float(sndFile, out, framesPerBuffer);

    if (framesRead < framesPerBuffer) {
        for (unsigned long i = framesRead; i < framesPerBuffer; ++i) {
            out[i] = 0.0f;
        }
        return paComplete; // Signal that playback is done
    }

    return paContinue;
}


class AudioPlayer {
public:
    AudioPlayer(const std::string& filePath): audioFile(0), audioStream(0) {
        if (Pa_Initialize() != paNoError) {
            std::cerr << "PortAudio initialization failed." << std::endl;
            return;
        }

        audioFile = sf_open(filePath.c_str(), SFM_READ, &sfInfo);
        if (!audioFile) {
            std::cerr << "Failed to open audio file: " << filePath << std::endl;
            return;
        }

        PaStreamParameters outputParameters;
        outputParameters.device = Pa_GetDefaultOutputDevice();
        outputParameters.channelCount = sfInfo.channels;
        outputParameters.sampleFormat = paFloat32;
        outputParameters.suggestedLatency = Pa_GetDeviceInfo(outputParameters.device)->defaultLowOutputLatency;
        outputParameters.hostApiSpecificStreamInfo = 0;

        if (Pa_OpenStream(&audioStream, 0, &outputParameters, sfInfo.samplerate, paFramesPerBufferUnspecified, paClipOff, audioCallback, audioFile) != paNoError) {
            std::cerr << "Failed to open PortAudio stream." << std::endl;
            return;
        }
    }
    
    ~AudioPlayer() {
        if (audioFile) {
            sf_close(audioFile);
        }

        if (audioStream) {
            Pa_StopStream(audioStream);
            Pa_CloseStream(audioStream);
            Pa_Terminate();
        }
    }

    bool start() {
        if (audioFile && audioStream) {
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

private:
    SNDFILE* audioFile;
    PaStream* audioStream;
    SF_INFO sfInfo;
};


int main() {
    const std::string filePath = "/home/abooker/Music/pool/looped/looped_55_si_2025-02-23_154447.wav";

    AudioPlayer audioPlayer(filePath);

    if (audioPlayer.start()) {
        audioPlayer.stop();
    }
   
    return 0;
}
