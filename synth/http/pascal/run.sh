rm -rf bin
mkdir -p bin
fpc server.pas
if [[ $? != 0 ]]
then
    exit $?
fi
mv server.o bin
./server $1
