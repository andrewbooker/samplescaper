#include "soundSource.h"

#include <string>
#include <sstream>
#include <vector>
#include <iostream>
#include <curl/curl.h>
#include <cstring>
#include <chrono>


using std::chrono::high_resolution_clock;
using std::chrono::duration_cast;
using std::chrono::milliseconds;



class OptionsProvider {
    typedef std::vector<std::string> t_options;
    t_options options;
    unsigned int pos;

public:
    OptionsProvider() : pos(0) {}

    void add(const std::string& o) {
        options.push_back(o);
    }

    const std::string& next() {
        return options.at((pos++) % options.size());
    }
};


class HttpSoundSource : public SoundSource {
public:
    typedef std::vector<std::string> t_hosts;

private:
    typedef std::vector<unsigned char> t_buffer;
    t_buffer buffer;
    const int idx;
    unsigned long pos;
    const std::vector<unsigned short> key;
    OptionsProvider& hosts;

    static size_t write(void* ptr, size_t size, size_t nmemb, void* stream) {
        t_buffer& out(*reinterpret_cast<t_buffer*>(stream));
        size_t read(size * nmemb);
        out.reserve(out.size() + read);
        unsigned char* samples(reinterpret_cast<unsigned char*>(ptr));
        out.insert(out.end(), samples, samples + read);
        return read;
    }

    void silence() {
        const float dur(1.0 + (rand() * 3.0 / RAND_MAX));
        std::cout << idx << " silence for " << dur << "s\n";
        const unsigned int len(dur * 44100 * sizeof(float));
        buffer.assign(len, 0);
    }

protected:
    bool fetchContent() {
        CURL* curl(curl_easy_init());
        buffer.clear();
        pos = 0;
        std::stringstream uri;
        uri << "http://" << hosts.next() << "/?note=" << key.at(rand() % key.size());
        if ((rand() * 1.0 / RAND_MAX) > 0.7) {
            silence();
            return true;
        }
        if (curl) {
            std::cout << idx << " fetching from " << uri.str() << std::endl;
            const auto start(high_resolution_clock::now());
            curl_easy_setopt(curl, CURLOPT_URL, uri.str().c_str());
            curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write);
            curl_easy_setopt(curl, CURLOPT_WRITEDATA, &buffer);

            CURLcode res(curl_easy_perform(curl));
            if (res != CURLE_OK) {
                std::cerr << curl_easy_strerror(res) << std::endl;
            }
            curl_easy_cleanup(curl);
            const auto fetchTime(duration_cast<milliseconds>(high_resolution_clock::now() - start));
            std::cout << idx << " read " << buffer.size() << " bytes in " << fetchTime.count() << "ms" << std::endl;
            return true;
        } else {
            std::cerr << "Failed to initialize curl\n";
        }
        return false;
    }

public:
    HttpSoundSource(const unsigned int i, OptionsProvider& h) :
        hosts(h),
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

