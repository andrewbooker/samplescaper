#include <iostream>
#include <sys/socket.h>
#include <netinet/in.h>
#include <unistd.h>
#include <sstream>
#include <string>
#include <regex>
#include <curl/curl.h>
#include <chrono>

using std::chrono::high_resolution_clock;
using std::chrono::duration_cast;
using std::chrono::milliseconds;


class Server {
    const int server_fd;
    struct sockaddr_in address;
    socklen_t addrlen;
    typedef std::vector<unsigned char> t_buffer;
    typedef std::vector<unsigned int> t_ports;
    t_ports ports;
    unsigned int lastPort;

    static size_t write(void* ptr, size_t size, size_t nmemb, void* stream) {
        t_buffer& out(*reinterpret_cast<t_buffer*>(stream));
        size_t read(size * nmemb);
        out.reserve(out.size() + read);
        unsigned char* samples(reinterpret_cast<unsigned char*>(ptr));
        out.insert(out.end(), samples, samples + read);
        return read;
    }

    void fetchInto(t_buffer& buffer, const unsigned int port, const unsigned short note) {
        std::stringstream uri;
        uri << "http://localhost:" << port << "/?note=" << note;
        CURL* curl(curl_easy_init());
        if (curl) {
            std::cout << "fetching from " << uri.str() << std::endl;
            const auto start(high_resolution_clock::now());
            curl_easy_setopt(curl, CURLOPT_URL, uri.str().c_str());
            curl_easy_setopt(curl, CURLOPT_USERAGENT, "Randomatone Distributor");
            curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write);
            curl_easy_setopt(curl, CURLOPT_WRITEDATA, &buffer);

            CURLcode res(curl_easy_perform(curl));
            const bool success(res == CURLE_OK);
            if (!success) {
                std::cerr << "Fetch failure: " << curl_easy_strerror(res) << std::endl;
            }
            curl_easy_cleanup(curl);
            if (success) {
                const auto fetchTime(duration_cast<milliseconds>(high_resolution_clock::now() - start));
                std::cout << "read " << buffer.size() << " bytes ("
                        << buffer.size() / (4 * 44100) << "s) in " << fetchTime.count()
                        << "ms at " << buffer.size() / (1024.0 * fetchTime.count()) << " MB/ms" << std::endl;
            }
        } else {
            std::cerr << "Failed to initialize curl\n";
        }
    }
    
    const unsigned int nextPort() {
        lastPort += 1;
        if (lastPort >= ports.size()) {
            lastPort = 0;
        }
        return ports.at(lastPort);
    } 

public:
    Server(const unsigned int port) : lastPort(0), addrlen(sizeof(address)), server_fd(socket(AF_INET, SOCK_STREAM, 0)) {
        ports.push_back(9961);
        ports.push_back(9962);
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
        const unsigned short note(atoi(last.c_str()));
        const unsigned int port(nextPort());
        t_buffer sound;
        fetchInto(sound, port, note);
        std::cout << "Received " << note << ". Sending request to to " << port << "\n";
        std::stringstream responseHeader;
        responseHeader << "HTTP/1.1 200 OK\r\n" << "Content-Type: application/octet-stream\r\n" << "Content-Length: " << sound.size() << "\r\n";
        responseHeader << "\r\n";
        const std::string& resp(responseHeader.str());
        send(client_fd, resp.c_str(), resp.size(), 0);
        send(client_fd, sound.data(), sound.size(), 0);

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
