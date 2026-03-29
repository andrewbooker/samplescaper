set -e
multiplexer_port=9964
rm -rf bin
mkdir -p bin
ghc -O2 --make server.hs -o bin/server
rm *.hi
rm *o
curl localhost:$multiplexer_port/add=$1
trap "curl localhost:$multiplexer_port/remove=$1" EXIT
bin/server $1
