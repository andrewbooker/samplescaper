#include "soundSource.h"
#include <string>
#include <vector>
#include <filesystem>
#include <sndfile.h>


class DiskSource : public SoundFileSource {
private:
    const std::string location;
    typedef std::vector<std::string> t_fileNames;
    t_fileNames fileNames;

protected:
    SNDFILE* getSoundFile() {
        for (const auto & entry : std::filesystem::directory_iterator(location)) {
            fileNames.push_back(entry.path());
        }
        const unsigned int selection(rand() % fileNames.size());
        SF_INFO info;
        return sf_open(fileNames[selection].c_str(), SFM_READ, &info);
    }

public:
    DiskSource(const unsigned int i) : location("/home/abooker/Music/pool/looped") {}
};

