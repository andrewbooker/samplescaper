. $(dirname $(pwd))/run_common.sh
if [ ! -f server ] || [ '-r' = "$2" ]; then
    ./compile.sh
fi
./server $1
