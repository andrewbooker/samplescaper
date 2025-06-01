rm server
rm server.o
fpc server.pas
if [[ $? != 0 ]]
then
    exit $?
fi
./server $1
