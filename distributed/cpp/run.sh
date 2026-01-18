mkdir -p bin
set -e
g++ server.cpp -o ./bin/server -l sndfile
port=$1
loc=$2
bin/server $port $loc
