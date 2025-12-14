set -e
links=("-l sndfile" "-l portaudio" "-l curl")
if [[ ! -v $(uname -a | grep raspberry) ]]; then
    links+=("-pthread")
fi
g++ player.cpp -o player ${links[@]}
./player "$@"
