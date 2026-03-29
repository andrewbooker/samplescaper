
rm -rf bin
mkdir -p bin
ghc -O2 --make server.hs -o bin/server
rm *.hi
rm *o
curl localhost:9964/add=$1
trap "curl localhost:9964/remove=$1" EXIT
bin/server $1
