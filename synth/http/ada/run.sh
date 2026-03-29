. ../run_common.sh
mkdir -p bin
gnatmake server.adb -D bin
./server $1
