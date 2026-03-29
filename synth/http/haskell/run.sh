set -e
rm -rf bin
mkdir -p bin
ghc -O2 --make server.hs -o bin/server
rm *.hi
rm *o
bin/server "$@"
