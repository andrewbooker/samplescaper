set -e
links=()
links+=("-l sndfile")
links+=("-l asound")
links+=("-l portaudio")
links+=("-l curl")
if [[ ! -v $(uname -a | grep raspberry) ]]; then
    links+=("-pthread")
fi
g++ player.cpp -o player ${links[@]}
./player "$@"
