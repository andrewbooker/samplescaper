rm -rf bin server
mkdir -p bin
gnatmake server.adb -D bin
