#pragma once

#include <sndfile.h>
#include <thread>
#include <iostream>


class SoundSource {
protected:
    bool ready;

    virtual bool fetchContent() = 0;
private:
    std::thread loop;
    bool closing;

    void fetch() {
        while (!closing) {
            if (!ready) {
                ready = fetchContent();
            } else {
               std::this_thread::sleep_for(std::chrono::milliseconds(500));
            }
        }
    }

    static void fetchLoop(SoundSource* s) {
        s->fetch();
    }

public:
    SoundSource() : ready(false), closing(false), loop(fetchLoop, this) {}
    virtual void readInto(float* out, const unsigned long sampleLength) = 0;
    virtual ~SoundSource() {
        closing = true;
        std::cout << "Stopping fetch loop... ";
        loop.join();
        std::cout << "stopped\n";
    }
};


class SoundFileSource : public SoundSource {
private:
    SNDFILE* soundFile;

protected:
    virtual SNDFILE* getSoundFile() = 0;

    bool fetchContent() {
        if (soundFile) {
            sf_close(soundFile);
            soundFile = 0;
        }
        soundFile = getSoundFile();
        if (soundFile) {
            return true;
        } else {
            std::cout << "Fetch thread failed to get sound file" << std::endl;
            std::this_thread::sleep_for(std::chrono::seconds(3));
        }
        return false;
    }

public:
    SoundFileSource() : soundFile(0) {}

    virtual ~SoundFileSource() {
        std::cout << "Closing sound file" << std::endl;
        if (soundFile) {
            sf_close(soundFile);
        }
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

