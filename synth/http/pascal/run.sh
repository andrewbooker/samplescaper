. ../run_common.sh
rm -rf bin
mkdir -p bin
fpc server.pas
mv server.o bin
./server $1
