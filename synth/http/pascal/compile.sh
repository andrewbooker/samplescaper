rm -rf bin server
mkdir -p bin
fpc server.pas
mv server.o bin
