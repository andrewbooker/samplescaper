#pragma once

#include <sndfile.h>
#include <thread>
#include <iostream>

class SoundSource {
private:
    bool ready;
    bool closing;
    SNDFILE* soundFile;
    std::thread loop;

    void fetch() {
        while (!closing) {
            if (!ready) {
                if (soundFile) {
                    sf_close(soundFile);
                    soundFile = 0;
                }
                soundFile = getSoundFile();
                if (soundFile) {
                    ready = true;
                } else {
                    std::cout << "Fetch thread failed to get sound file" << std::endl;
                    std::this_thread::sleep_for(std::chrono::seconds(3));
                }
            } else {
               std::this_thread::sleep_for(std::chrono::milliseconds(500));
            }
        }
    }

    static void fetchLoop(SoundSource* s) {
        s->fetch();
    }

protected:
    virtual SNDFILE* getSoundFile() = 0;

public:
    SoundSource() : ready(false), closing(false), soundFile(0), loop(fetchLoop, this) {}
    virtual ~SoundSource() {
        if (soundFile) {
            sf_close(soundFile);
        }
        closing = true;
        std::cout << "Stopping fetch loop... ";
        loop.join();
        std::cout << "stopped\n";
    }

    void readInto(float* out, const unsigned long sampleLength) {
        if (!ready) {
            memset(out, 0, sampleLength * sizeof(float));
            return;
        }
        const unsigned long read(sf_readf_float(soundFile, out, sampleLength));
        ready = (read == sampleLength);
    }
};

