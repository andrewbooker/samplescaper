#pragma once

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



