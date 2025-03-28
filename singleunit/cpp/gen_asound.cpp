#include <fstream>

int main() {
    std::ofstream out("asound.conf", std::ofstream::out);

    out << "{}\n";
   
    return 0;
}
