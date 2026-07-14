rm -rf bin
mkdir -p bin
ghc -O2 --make server.hs -o ./server
rm *.hi
rm *o
