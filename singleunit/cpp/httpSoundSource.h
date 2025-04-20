#include "soundSource.h"

#include <string>
#include <sstream>
#include <vector>
#include <iostream>
#include <curl/curl.h>
#include <cstring>


class HttpSoundSource : public SoundSource {
private:
    const std::string url;
    typedef std::vector<unsigned char> t_buffer;
    t_buffer buffer;
    const int idx;
    unsigned long pos;
    const std::vector<unsigned short> key;

    static size_t write(void* ptr, size_t size, size_t nmemb, void* stream) {
        t_buffer& out(*reinterpret_cast<t_buffer*>(stream));
        size_t read(size * nmemb);
        out.reserve(out.size() + read);
        unsigned char* samples(reinterpret_cast<unsigned char*>(ptr));
        out.insert(out.end(), samples, samples + read);
        return read;
    }

protected:
    bool fetchContent() {
        CURL* curl(curl_easy_init());
        buffer.clear();
        pos = 0;
        std::stringstream uri;
        uri << url << "/?note=" << key.at(rand() % key.size());
        if (curl) {
            std::cout << idx << " fetching from " << uri.str() << std::endl;
            curl_easy_setopt(curl, CURLOPT_URL, uri.str().c_str());
            curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write);
            curl_easy_setopt(curl, CURLOPT_WRITEDATA, &buffer);

            CURLcode res(curl_easy_perform(curl));
            if (res != CURLE_OK) {
                std::cerr << curl_easy_strerror(res) << std::endl;
            }
            curl_easy_cleanup(curl);
            std::cout << idx << " read " << buffer.size() << " bytes" << std::endl;
            return true;
        } else {
            std::cerr << "Failed to initialize curl\n";
        }
        return false;
    }

public:
    HttpSoundSource(const unsigned int i) :
        url("http://0.0.0.0:9964"),
        idx(i),
        pos(0),
        key {57, 59, 60, 62, 64, 65, 67, 69} {}

    void readInto(float* out, const unsigned long sampleLength) {
        memset(out, 0, sampleLength * sizeof(float));
        if (!ready) {
            return;
        }
        const unsigned long byteLength(sampleLength * sizeof(float));
        const unsigned long toRead(std::min(byteLength, buffer.size() - pos));
        memcpy(out, buffer.data() + pos, toRead);
        pos += toRead;
        ready = (toRead == byteLength);
    }
};

