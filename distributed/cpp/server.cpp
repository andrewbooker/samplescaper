#include <iostream>
#include <cstring>
#include <sys/socket.h>
#include <netinet/in.h>
#include <unistd.h>
#include <sstream>
#include <string>
#include <cmath>
#include <math.h>
#include <vector>
#include <filesystem>
#include <sndfile.h>


#define SAMPLE_RATE 44100

const float anywhereBetween(const float& l, const float& u) {
    const float r(rand() * 1.0 / RAND_MAX);
    return l + (std::pow(r * (u - l), 2.0) / u);
}


class Envelope {
public:
    virtual const float at(const unsigned long i) const = 0;
};


class Merged : public Envelope {
private:
    const Envelope& env1;
    const Envelope& env2;
public:
    Merged(const Envelope& e1, const Envelope& e2) : env1(e1), env2(e2) {}
    const float at(const unsigned long i) const {
        return env1.at(i) * env2.at(i);
    }
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
    const unsigned long rampUp;
    const unsigned long rampDown;
    const unsigned long startRampDown;

public:
    RampUpDown(const unsigned long size) :
        rampUp(SAMPLE_RATE * anywhereBetween(2.0, 4.0)),
        rampDown(size * anywhereBetween(0.2, 0.5)),
        startRampDown(size - rampDown)
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


typedef std::vector<float> t_sound;

class SampleRepeater {
private:
    const t_sound& source;
    const unsigned long length;
    const unsigned long xFade;
    const unsigned long usableLength;

    const float sampleAt(const unsigned long s) const {
        if (s < xFade) {
            const float f(s * 1.0 / xFade);
            return (f * source[s]) + ((1.0 - f) * source[s + length - xFade]);
        }
        return source[s];
    }

public:
    SampleRepeater(const t_sound& s) :
        source(s),
        length(s.size()),
        xFade(anywhereBetween(0.1, 0.4) * length),
        usableLength(length - xFade) {}

    t_sound& onto(t_sound& buffer, const unsigned long size, const Envelope& am) const {
        for (unsigned long i(0); i != size; ++i) {
            buffer.push_back(am.at(i) * sampleAt(i % usableLength));
        }
        return buffer;
    }
};


class SampleFetcher {
private:
    typedef std::vector<std::string> t_fileNames;
    t_sound buffer;
    const std::string location;
    std::string latest;

    void loadFileInto(t_sound& fileBuffer, const std::string& f) const {
        SF_INFO info;
        SNDFILE* soundFile(sf_open(latest.c_str(), SFM_READ, &info));
        const unsigned long sampleLength(info.frames);
        fileBuffer.reserve(sampleLength);
        fileBuffer.assign(sampleLength, 0.0);
        const unsigned long read(sf_readf_float(soundFile, fileBuffer.data(), sampleLength));
        sf_close(soundFile);
    }

public:
    SampleFetcher(const char* loc) : location(loc), latest("") {}

    const std::string& describeLatest() const {
        return latest;
    }

    const t_sound& fetch() {
        const float lengthSecs(anywhereBetween(8, 20));
        const unsigned long size(SAMPLE_RATE * lengthSecs);
        buffer.clear();
        buffer.reserve(size);

        const Lfo amLfo;
        const AsPositive amLfoP(amLfo);
        const float amDepth(anywhereBetween(0.0, 1.0));
        const Scaled am(amLfo, amDepth, 1.0 - amDepth);

        const RampUpDown ramp(size);
        const Merged amplitude(am, ramp);

        t_fileNames fileNames;
        for (const auto & entry : std::filesystem::directory_iterator(location)) {
            fileNames.push_back(entry.path());
        }
        const unsigned int selection(rand() % fileNames.size());
        t_sound fileBuffer;
        latest = fileNames[selection];
        loadFileInto(fileBuffer, latest);
        SampleRepeater repeat(fileBuffer);
        return repeat.onto(buffer, size, amplitude);
    }
};


class Server {
    const int server_fd;
    const char* fileLoc;
    struct sockaddr_in address;
    socklen_t addrlen;

public:
    Server(const unsigned int port, const char* fl) : fileLoc(fl), addrlen(sizeof(address)), server_fd(socket(AF_INET, SOCK_STREAM, 0)) {
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
        char ignore[1024] {};
        read(client_fd, ignore, sizeof(ignore));

        SampleFetcher fetcher(fileLoc);
        const t_sound& sound(fetcher.fetch());
        const unsigned long binarySize(sound.size() * sizeof(float));
        std::cout << "sending " << fetcher.describeLatest() << " for " << sound.size() * 1.0 / SAMPLE_RATE << "s (" << binarySize << " bytes)\n";
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
    const char* fileLoc(argv[2]);
    Server server(port, fileLoc);
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
