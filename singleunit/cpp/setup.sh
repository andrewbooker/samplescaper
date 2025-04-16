sudo apt install -y libsndfile1-dev
sudo apt install -y portaudio19-dev
sudo apt install -y libgtest-dev
sudo apt install -y cmake
sudo apt install -y libcurl4-openssl-dev
cd /usr/src/gtest
sudo cmake .
sudo make
sudo cp lib/*.a /usr/lib
cd -
