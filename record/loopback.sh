#!/bin/bash
set -e
port=9753
echo "Listening on port $port"

recDir=~/Music/recording
mkdir -p $recDir
state=$(mktemp)
echo 0 > $state


stop() {
    pid=$(cat $state)
    if [ $pid -gt 0 ]
    then
        kill $pid 
    fi
}


startStop() {
    s=$(cat $state)
    if [ $s -eq 0 ]
    then
        audioFn=${recDir}/randomatones_$(date +"%Y%m%d_%H%M%S").wav
        ffmpeg -f alsa -channels 2 -sample_rate 44100 -i loopout $audioFn &
        echo $! > $state
    else
        stop
        echo 0 > $state
    fi
}

message() {
    s=$(cat $state)
    if [ $s -eq 0 ]
    then
        echo "starting recording"
    else
        echo "stopping recording process $s"
    fi
}

respond() {
    r="$(message)\n"
    h="Content-Type: text/plain\r\nContent-Length: ${#r}\r\n"
    echo -e "HTTP/1.1 200 OK\r\n$h\r\n$r"
}

serve() {
    while true; do
        respond | nc -l $port | grep GET
        startStop
    done
}

finish() {
    echo ''
    stop
    rm $state
    
    echo "Finished"
}

trap finish EXIT
serve



