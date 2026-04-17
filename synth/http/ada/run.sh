. "$(dirname $(pwd))/run_common.sh"
mkdir -p bin
rm -rf bin/*
rm -f server
gnatmake server.adb -D bin
./server $1
