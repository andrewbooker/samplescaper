#!/bin/bash
set -e
if [[ "$(aplay -l | grep Loopback)" != *'Loopback'* ]]; then
    echo 'setting up loopback (requires sudo)'
    sudo modprobe snd-aloop
fi
