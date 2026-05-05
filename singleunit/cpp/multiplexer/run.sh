compile() {
    echo 'compiling multiplexer'
    set -e
    mkdir -p bin
    g++ server.cpp -l curl -o ./bin/server
}
[ -f ./bin/server ] || compile
bin/server $1
