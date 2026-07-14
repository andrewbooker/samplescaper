set -e
rm -rf bin
mkdir -p bin
g++ server.cpp -l curl -o ./server
