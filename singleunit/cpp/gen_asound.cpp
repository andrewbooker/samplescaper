#include <fstream>
#include <vector>
#include <sstream>

int main() {
    const std::vector<int> cards {0};
    const std::string cardName("Audigy2");
    const std::vector<int> devices {0, 3, 4};
    const unsigned int deviceChannels(2);
    const std::string name("soundblaster");
    const std::string sp("  ");

    const unsigned int channels(deviceChannels * cards.size() * devices.size());
    std::ofstream out("asound.conf", std::ofstream::out);

    for (std::vector<int>::const_iterator d(devices.begin()); d != devices.end(); ++d) {
        out << "pcm." << name << *d << " {\n";
        out << sp << "type hw\n";
        out << sp << "card \"" << cardName << "\"\n";
        out << sp << "device " << *d << "\n";
        out << "}";
	out << "\n\n";
    }

    std::stringstream slaveName;
    slaveName << name << "_" << channels << "ch";

    std::vector<std::string> bindings;

    out << "pcm." << slaveName.str() << " {\n";
    out << sp << "type multi\n";
    out << sp << "slaves [\n";
    for (unsigned int c(0); c != cards.size(); ++c) {
        for (unsigned int d(0); d != devices.size(); ++d) {
            const unsigned int slave(c + d);
            out << sp << sp << "{\n";
            out << sp << sp << sp << "pcm \"" << name << devices[d] << "\"\n";
            out << sp << sp << sp << "channels " << deviceChannels << "\n";
            out << sp << sp << "}\n";
            for (unsigned int ch(0); ch != deviceChannels; ++ch) {
                unsigned int cc(((c + d) * deviceChannels) + ch);
		std::stringstream b;
                b << "{ slave " << slave << " channel " << ch << " }";
                bindings.push_back(b.str());
	    }
	}
    }
    out << sp << "]\n";
    out << sp << "bindings [\n";
    for (std::vector<std::string>::const_iterator i(bindings.begin()); i != bindings.end(); ++i) {
        out << sp << sp << *i << "\n";
    }
    out << sp << "]\n";
    out << "}";

    out << "\n";
    out << "\n";

    out << "pcm.randomatones {\n";
    out << sp << "type route\n";
    out << sp << "slave.pcm \"" << slaveName.str() << "\"\n";
    for (unsigned int c(0); c != channels; ++c) {
        out << sp << "ttable." << c << "." << c << " 1\n";
    }
    out << "}";
    out << "\n";
    return 0;
}
