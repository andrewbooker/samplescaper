#!/bin/bash

die() {
    echo $1
    exit 1
}

test_ssh_to() {
    host=$1
    if [[ -n $(nc -zv $host 22 2>&1 | grep succeeded) ]]
    then
        echo "can reach $host for ssh"
    else
        die "cannot reach $host"
    fi
}

if [[ -n $(hostname -I | grep '1\.88') ]]
then
    echo 'localhost correct IP'
else
    die 'incorrect localhost IP address '
fi

test_ssh_to '192.168.1.99'

echo 'all good'
