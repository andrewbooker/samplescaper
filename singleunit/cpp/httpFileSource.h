#include "soundFileSource.h"

#include <string>
#include <vector>
#include <iostream>
#include <curl/curl.h>
#include <cstring>
#include <sndfile.h>


struct MemoryBuffer {
    const unsigned char* data;
    sf_count_t size;
    sf_count_t pos;
};


sf_count_t vio_get_filelen(void* user_data) {
    auto* mem(static_cast<MemoryBuffer*>(user_data));
    return mem->size;
}

sf_count_t vio_seek(sf_count_t offset, int whence, void* user_data) {
    auto* mem(static_cast<MemoryBuffer*>(user_data));
    sf_count_t newpos(mem->pos);
    switch (whence) {
        case SEEK_SET: newpos = offset; break;
        case SEEK_CUR: newpos += offset; break;
        case SEEK_END: newpos = mem->size + offset; break;
        default: return -1;
    }
    if (newpos < 0 || newpos > mem->size) return -1;
    mem->pos = newpos;
    return mem->pos;
}

sf_count_t vio_read(void* ptr, sf_count_t count, void* user_data) {
    auto* mem(static_cast<MemoryBuffer*>(user_data));
    sf_count_t remaining(mem->size - mem->pos);
    if (count > remaining) count = remaining;
    memcpy(ptr, mem->data + mem->pos, count);
    mem->pos += count;
    return count;
}

sf_count_t vio_write(const void*, sf_count_t, void*) {
    // Not implemented for read-only
    return 0;
}

sf_count_t vio_tell(void* user_data) {
    auto* mem(static_cast<MemoryBuffer*>(user_data));
    return mem->pos;
}

SF_VIRTUAL_IO vio = {
    vio_get_filelen,
    vio_seek,
    vio_read,
    vio_write,
    vio_tell
};

class HttpFileSource : public SoundFileSource {
private:
    const std::string url;
    typedef std::vector<unsigned char> t_buffer;
    t_buffer buffer;
    const int idx;
    MemoryBuffer mem;

    static size_t write(void* ptr, size_t size, size_t nmemb, void* stream) {
        t_buffer& out(*reinterpret_cast<t_buffer*>(stream));
        size_t read(size * nmemb);
        out.reserve(out.size() + read);
        unsigned char* samples(reinterpret_cast<unsigned char*>(ptr));
        out.insert(out.end(), samples, samples + read);
        return read;
    }

protected:
    SNDFILE* getSoundFile() {
        CURL* curl(curl_easy_init());
        buffer.clear();
        if (curl) {
            std::cout << idx << " fetching from " << url << std::endl;
            curl_easy_setopt(curl, CURLOPT_URL, url.c_str());
            curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write);
            curl_easy_setopt(curl, CURLOPT_WRITEDATA, &buffer);

            CURLcode res(curl_easy_perform(curl));
            if (res != CURLE_OK) {
                std::cerr << curl_easy_strerror(res) << std::endl;
            }
            curl_easy_cleanup(curl);
            std::cout << "read " << buffer.size() << " bytes" << std::endl;

            mem.data = buffer.data();
            mem.size = buffer.size();
            mem.pos = 0;
            SF_INFO sfinfo;
            SNDFILE* sf(sf_open_virtual(&vio, SFM_READ, &sfinfo, &mem));
            return sf;
        } else {
            std::cerr << "Failed to initialize curl\n";
        }
        return 0;
    }

public:
    HttpFileSource(const unsigned int i) : url("http://0.0.0.0:3064"), idx(i) {
        memset(&mem, 0, sizeof(MemoryBuffer));
    }
};

