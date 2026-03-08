set -e
mkdir -p bin
g++ server.cpp -l curl -o ./bin/server
bin/server $1
