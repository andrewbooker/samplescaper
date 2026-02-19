#!/bin/bash

startAfter='2318'
stopAfter='2322'

timeNow=$(date +%H%M)
if [ "$timeNow" \> "$stopAfter" ]
then
    echo "stopped at $timeNow"
    tmux send-keys -t 1 'p'
elif [ "$timeNow" \> "$startAfter" ]
then
    echo "running at $timeNow"
    tmux send-keys -t 1 'r'
fi
