#!/bin/bash

startAt="$1"
stopAt="$2"

timeNow=$(date +%H:%M)
if [ ! "$timeNow" \< "$stopAt" ]
then
    echo "stopped at $timeNow"
    tmux send-keys -t 1 'p'
elif [ ! "$timeNow" \< "$startAt" ]
then
    echo "running at $timeNow"
    tmux send-keys -t 1 'r'
fi
