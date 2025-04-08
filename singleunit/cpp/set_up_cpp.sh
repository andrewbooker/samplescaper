sudo apt install -y libsndfile1-dev
sudo apt install -y portaudio19-dev
sudo apt install libgtest-dev
sudo apt install cmake
cd /usr/src/gtest
sudo cmake .
sudo make
sudo cp lib/*.a /usr/lib
cd -
