rm -f server; ghc -O2 --make server.hs -o server && ./server "$@"
