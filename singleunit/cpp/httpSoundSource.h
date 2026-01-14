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


class SoundPlayListener {
private:
    const unsigned int idx;
    unsigned int last;
    struct {
        char *response;
        size_t size;
    } dump;

    static size_t swallowResponse(void* ptr, size_t size, size_t nmemb, void* stream) {
        size_t read(size * nmemb);
        return read;
    }

    void send(const unsigned int v) {
        last = v;
        std::stringstream url;
        url << "http://localhost:9971/" << (v ? "start" : "stop") << "?" << idx;
        CURL* curl(curl_easy_init());
        curl_easy_setopt(curl, CURLOPT_URL, url.str().c_str());
        curl_easy_setopt(curl, CURLOPT_USERAGENT, "Randomatone");
        curl_easy_setopt(curl, CURLOPT_POST, 1);
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, swallowResponse);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, (void *)&dump);

        CURLcode res(curl_easy_perform(curl));
        const bool success(res == CURLE_OK);
        if (!success) {
            std::cerr << "Listener failure posting to " << url.str() << ": " << curl_easy_strerror(res) << std::endl;
        }
        curl_easy_cleanup(curl);
    }

public:
    SoundPlayListener(const unsigned int i) : idx(i), last(0) {
        dump = {0};
    }

    void on() {
        if (last == 1) {
            return;
        }
        std::cout << "playing " << idx << std::endl;
        send(1);
    }

    void off() {
        if (last == 0) {
            return;
        }
        std::cout << "stopping " << idx << std::endl;
        send(0);
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
    SoundPlayListener listener;
    bool isSilent;

    static size_t write(void* ptr, size_t size, size_t nmemb, void* stream) {
        t_buffer& out(*reinterpret_cast<t_buffer*>(stream));
        size_t read(size * nmemb);
        out.reserve(out.size() + read);
        unsigned char* samples(reinterpret_cast<unsigned char*>(ptr));
        out.insert(out.end(), samples, samples + read);
        return read;
    }

    void silence() {
        const float dur(0.5 + (1.0 * rand() / RAND_MAX));
        std::cout << idx << " silence for " << dur << "s\n";
        const unsigned int len(dur * 44100 * sizeof(float));
        buffer.assign(len, 0);
    }

protected:
    bool fetchContent() {
        buffer.clear();
        pos = 0;
        if (isSilent) {
            silence();
            isSilent = false;
            return true;
        }
        std::stringstream uri;
        const int octave(12 * (rand() % 2));
        uri << "http://" << hosts.next() << "/?note=" << octave + (key.at(rand() % key.size()));
        CURL* curl(curl_easy_init());
        bool success(false);
        if (curl) {
            std::cout << idx << " fetching from " << uri.str() << std::endl;
            const auto start(high_resolution_clock::now());
            curl_easy_setopt(curl, CURLOPT_URL, uri.str().c_str());
            curl_easy_setopt(curl, CURLOPT_USERAGENT, "Randomatone");
            curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write);
            curl_easy_setopt(curl, CURLOPT_WRITEDATA, &buffer);

            CURLcode res(curl_easy_perform(curl));
            const bool success(res == CURLE_OK);
            if (!success) {
                std::cerr << "Fetch failure: " << curl_easy_strerror(res) << std::endl;
                isSilent = true;
            }
            curl_easy_cleanup(curl);
            if (success) {
                const auto fetchTime(duration_cast<milliseconds>(high_resolution_clock::now() - start));
                std::cout << idx << " read " << buffer.size() << " bytes ("
                        << buffer.size() / (4 * 44100) << "s) in " << fetchTime.count()
                        << "ms at " << buffer.size() / (1024.0 * fetchTime.count()) << " MB/ms" << std::endl;
                listener.on();
                isSilent = !isSilent;
            }
            return success;
        } else {
            std::cerr << "Failed to initialize curl\n";
        }
        return false;
    }

public:
    HttpSoundSource(const unsigned int i, OptionsProvider& h) :
        hosts(h),
        idx(i),
        listener(i),
        pos(0),
        isSilent(false),
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
        if (!ready) {
            listener.off();
        }
    }
};

