#include <iostream>
#include <cstring>
#include <sys/socket.h>
#include <netinet/in.h>
#include <unistd.h>
#include <sstream>
#include <string>
#include <regex>


class Server {
private:
    const int server_fd;
    struct sockaddr_in address;
    socklen_t addrlen;

public:
    Server(const unsigned int port) : addrlen(sizeof(address)), server_fd(socket(AF_INET, SOCK_STREAM, 0)) {
        if (server_fd == 0) {
            perror("Failed to create socket");
            exit(EXIT_FAILURE);
        }

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
        const unsigned int note(atoi(last.c_str()));

        std::cout << "Responding to {" << note << "}\n";
        const std::string r("hello Andrew");
        std::stringstream response;
        response << "HTTP/1.1 200 OK\r\n" << "Content-Type: text/plain\r\n" << "Content-Length: " << r.size() << "\r\n";
        response << "\r\n";
        response << r;

        const std::string& resp(response.str());
        send(client_fd, resp.c_str(), resp.size(), 0);
        close(client_fd);
        return false;
    }
};


int main() {
    const unsigned int port(9963);
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
