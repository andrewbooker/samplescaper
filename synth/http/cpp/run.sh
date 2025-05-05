mkdir -p bin
g++ server.cpp -o ./bin/server
if [ $? != 0 ]
then
    exit $?
fi
bin/server $1
