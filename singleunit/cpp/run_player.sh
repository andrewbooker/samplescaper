g++ player.cpp -o player -l sndfile -l portaudio -l curl
if [ $? == 1 ]
then
    exit
fi
./player "$@"
