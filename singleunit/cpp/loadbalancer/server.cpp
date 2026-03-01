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
        const short int port(9961);
        std::vector<float> sound;
        sound.push_back(note);
        const unsigned long binarySize(sound.size() * sizeof(float));
        std::cout << "Received " << port << ". Sending request to to " << port << "\n";
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
    std::cout << "Load balancer listening on port " << port << "...\n";
    bool done(false);
    while (!done) {
        done = server.listenLoop();
    }
    return 0;
}
