#pragma once

#include <thread>
#include <iostream>


class SoundSource {
protected:
    bool ready;

    virtual bool fetchContent() = 0;
private:
    std::thread loop;
    bool running;
    bool paused;

    void fetch() {
        while (running) {
            if (!ready && !paused) {
                ready = fetchContent();
            } else {
                std::this_thread::sleep_for(std::chrono::milliseconds(1000));
            }
        }
    }

    static void fetchLoop(SoundSource* s) {
        s->fetch();
    }

public:
    SoundSource() : ready(false), running(true), paused(false), loop(fetchLoop, this) {}
    void setPaused(const bool p) {
        paused = p;
    }
    virtual void readInto(float* out, const unsigned long sampleLength) = 0;
    virtual ~SoundSource() {
        running = false;
        std::cout << "Stopping fetch loop... ";
        loop.join();
        std::cout << "stopped\n";
    }
};



