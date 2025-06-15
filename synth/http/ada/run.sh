rm server
[[ -d ./server.ali ]] && rm server.ali
[[ -d ./server.o ]] && rm server.o

gnatmake server.adb
./server $1
