mkdir -p bin
set -e
g++ server.cpp -o ./bin/server -l sndfile
bin/server $1 ~/Music/pool/raw
