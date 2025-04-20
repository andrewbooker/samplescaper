#include <iostream>
#include <cstring>
#include <sys/socket.h>
#include <netinet/in.h>
#include <unistd.h>
#include <sstream>
#include <string>
#include <regex>
#include <cmath>
#include <math.h>


#define SAMPLE_RATE 44100

const float frequencyOf(const unsigned short n) {
    return std::pow(2, (n - 69) / 12.0) * 440;
}


const float anywhereBetween(const float& l, const float&u) {
    const float r(rand() * 1.0 / RAND_MAX);
    return l + (r * (u - l));
}


class Envelope {
public:
    virtual const float at(const unsigned long i) const = 0;
};


class ConstVal : public Envelope {
    const float& val;

public:
    ConstVal(const float& v) : val(v) {}
    const float at(const unsigned long i) const { return val; }
};


class Scaled : public Envelope {
    const Envelope& envelope;
    const float min;
    const float scale;

public:
    Scaled(const Envelope& e, const float m, const float s) : envelope(e), min(m), scale(s) {}

    const float at(const unsigned long i) const {
        return min + (scale * envelope.at(i));
    }
};


class AsPositive : public Envelope {
    const Envelope& oscillator;

public:
    AsPositive(const Envelope& e) : oscillator(e) {}

    const float at(const unsigned long i) const {
        return 0.5 * (1.0 + oscillator.at(i));
    }
};


class RampUpDown : public Envelope {
    const unsigned long size;
    const unsigned long rampUp;
    const unsigned long rampDown;
    const unsigned long startRampDown;

public:
    RampUpDown(const unsigned long s) :
        size(s),
        rampUp(SAMPLE_RATE * anywhereBetween(2.0, 4.0)),
        rampDown(anywhereBetween(0.2 * s, 0.5 * s)),
        startRampDown(s - rampDown)
    {}

    const float at(const unsigned long i) const {
        if (i < rampUp) {
            return 0.5 * (1.0 + cos(M_PI * (i + rampUp) / rampUp));
        }
        if (i > startRampDown) {
            return 0.5 * (1.0 + cos(M_PI * (i - startRampDown) / rampDown));
        }
        return 1.0;
    }
};


class Lfo : public Envelope {
    const float freq;

public:
    Lfo() : freq(anywhereBetween(0.05, 5.5)) {}

    const float at(const unsigned long i) const {
        return sin(2 * M_PI * freq * i / SAMPLE_RATE);
    }
};


class Synth {
public:
    typedef std::vector<float> t_sound;

private:
    t_sound buffer;
    const float& freq;

public:
    Synth(const float& f) : freq(f) {}

    const t_sound& generate() {
        const float lengthSecs(anywhereBetween(8, 15));
        const unsigned long size(SAMPLE_RATE * lengthSecs);
        buffer.clear();
        buffer.reserve(size);

        const Lfo phaseLfo;
        const Scaled phase(phaseLfo, 0.0, freq * anywhereBetween(0.0005, 0.008));

        const Lfo amLfo;
        const AsPositive amLfoP(amLfo);
        const float amDepth(anywhereBetween(0.0, 1.0));
        const Scaled am(amLfo, amDepth, 1.0 - amDepth);

        const RampUpDown amplitude(size);
        for (unsigned long i(0); i != size; ++i) {
            buffer.push_back(am.at(i) * amplitude.at(i) * sin(phase.at(i) + (2 * M_PI * freq * i / SAMPLE_RATE)));
        }
        return buffer;
    }
};


class Server {
    const int server_fd;
    struct sockaddr_in address;
    socklen_t addrlen;

public:
    Server(const unsigned int port) : addrlen(sizeof(address)), server_fd(socket(AF_INET, SOCK_STREAM, 0)) {
        if (server_fd == 0) {
            perror("Failed to create socket");
            exit(EXIT_FAILURE);
        }
        int opt(1);
        setsockopt(server_fd, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(opt));

        address.sin_family = AF_INET;
        address.sin_addr.s_addr = INADDR_ANY;
        address.sin_port = htons(port);

        if (bind(server_fd, reinterpret_cast<sockaddr*>(&address), sizeof(address)) < 0) {
            perror("bind failed");
            close(server_fd);
            exit(EXIT_FAILURE);
        }

        if (listen(server_fd, 3) < 0) {
            perror("listen failed");
            close(server_fd);
            exit(EXIT_FAILURE);
        }
    }

    ~Server() {
        if (isAlive()) {
            std::cout << "Shutting down\n";
            close(server_fd);
        }
    }

    const bool isAlive() const {
        return server_fd != 0;
    }

    bool listenLoop() {
        int client_fd(accept(server_fd, reinterpret_cast<sockaddr*>(&address), &addrlen));
        if (client_fd < 0) {
            return true;
        }
        char request[1024] {};
        read(client_fd, request, sizeof(request));
        const std::string req(request);

        std::regex rx("^([A-Z]+) /(\\?)?([a-z]+)=?([0-9]*)");
        std::smatch m;
        bool found(std::regex_search(req, m, rx));
        std::string last;
        for (std::string s : m) {
            if (!s.empty()) last = s;
        }
        if (last == std::string("die")) return true;
        const short int note(atoi(last.c_str()));
        const float f(frequencyOf(note));

        Synth synth(f);
        const Synth::t_sound& sound(synth.generate());
        std::cout << "Note " << note << " at " << f << "Hz for " << sound.size() * 1.0 / SAMPLE_RATE << "s\n";
        const unsigned long binarySize(sound.size() * sizeof(float));
        std::stringstream responseHeader;
        responseHeader << "HTTP/1.1 200 OK\r\n" << "Content-Type: application/octet-stream\r\n" << "Content-Length: " << binarySize << "\r\n";
        responseHeader << "\r\n";
        const std::string& resp(responseHeader.str());
        send(client_fd, resp.c_str(), resp.size(), 0);
        send(client_fd, sound.data(), binarySize, 0);

        close(client_fd);
        return false;
    }
};


int main(int argc, char* argv[]) {
    srand(time(0));
    if (argc < 2) {
        std::cout << "Must supply port number" << std::endl;
        return 0;
    }
    const unsigned int port(atoi(argv[1]));
    Server server(port);
    if (!server.isAlive()) {
        return 1;
    }
    std::cout << "Server listening on port " << port << "...\n";
    bool done(false);
    while (!done) {
        done = server.listenLoop();
    }
    return 0;
}
