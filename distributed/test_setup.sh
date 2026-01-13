#!/bin/bash

die() {
    echo $1
    exit 1
}


assert_success() {
    if [[ -n $1 ]]
    then
        echo "can $2"
    else
        die "cannot $2"
    fi
}

test_ssh_to() {
    host=$1
    assert_success "$(nc -zv $host 22 2>&1 | grep succeeded)" "reach $host for ssh"
}

test_has_sound_device() {
    host=$1
    sd=$(ssh pi@$host "~/Documents/samplescaper/singleunit/play.py" | grep randomatones)
    assert_success "$sd" "play sounds on $host"
}


assert_success "$(hostname -I | grep '1\.88')" "see correct localhost IP address"
test_ssh_to '192.168.1.99'
test_has_sound_device '192.168.1.99'

echo 'all good'
