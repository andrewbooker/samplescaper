g++ player.cpp -o player -l sndfile -l portaudio -l curl
./player "$@"
