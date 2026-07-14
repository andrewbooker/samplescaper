set -e
if [ ! -f player/player ]; then
    cd player
    ./compile.sh
    cd -
fi
player/player "$@"
