#include <string>
#include <vector>
#include <iostream>
#include <sndfile.h>
#include <curl/curl.h>


class HttpSource {
private:
    const std::string url;
    SNDFILE* soundFile;
    typedef std::vector<unsigned char> t_buffer;
    t_buffer buffer;

    static size_t write_to_file(void* ptr, size_t size, size_t nmemb, void* stream) {
        t_buffer& out(*reinterpret_cast<t_buffer*>(stream));
        size_t read(size * nmemb);
        out.reserve(out.size() + read);
        unsigned char* samples(reinterpret_cast<unsigned char*>(ptr));
        out.insert(out.end(), samples, samples + read);
        return read;
    }
public:
    HttpSource() : url("http://0.0.0.0:3064") {}

    void fetch() {
        CURL* curl(curl_easy_init());
        if (curl) {
            std::cout << "fetching from " << url << std::endl;
            curl_easy_setopt(curl, CURLOPT_URL, url.c_str());
            curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_to_file);
            curl_easy_setopt(curl, CURLOPT_WRITEDATA, &buffer);

            CURLcode res(curl_easy_perform(curl));
            if (res != CURLE_OK) {
                std::cerr << curl_easy_strerror(res) << std::endl;
            }
            curl_easy_cleanup(curl);
        } else {
            std::cerr << "Failed to initialize curl\n";
        }
        std::cout << buffer.size() << std::endl;
    }
};

