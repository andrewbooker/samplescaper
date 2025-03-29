#include <fstream>
#include <vector>

int main() {
    const std::vector<int> cards {1, 2};
    const std::vector<int> subdevices {0};
    const int deviceChannels(2);
    const std::string name("maestro");
    const std::string sp("  ");

    std::ofstream out("asound.conf", std::ofstream::out);

    out << "pcm." << name << " {\n";

    out << "}";

    out << "\n";
    out << "\n";

    out << "pcm.randomatones {\n";
    out << sp << "type route\n";
    out << sp << "slave.pcm \"" << name << "\"\n";
    const unsigned int channels(deviceChannels * cards.size() * subdevices.size());
    for (unsigned int c(0); c != channels; ++c) {
        out << sp << "ttable." << c << "." << c << " 1\n";
    }
    out << "}";
    out << "\n";
    return 0;
}
