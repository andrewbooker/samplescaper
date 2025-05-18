#!/usr/bin/bash

declare -A cards=()
cards["hdmi"]=8
cards["front"]=2
cards["loopin"]=2
inputChannels=8
outputGain=0.3
indent='    '

plug() {
    name=$1
    dev="$2"
    echo "pcm.$name {"
	echo "${indent}type plug"
	echo "${indent}slave.pcm \"$dev\""
    echo '}'
    echo ''
}


echo "pcm.multi {"
echo "${indent}type route"
echo "${indent}slave.pcm {"
echo "${indent}${indent}type multi"

outputs=${#cards[@]}
declare -A slaves=()
letter='a'

for c in ${!cards[@]}; do
    ch=${cards[$c]}
    slaves["${letter}"]=$ch

    echo "${indent}${indent}slaves.${letter}.pcm \"${c}\""
    echo "${indent}${indent}slaves.${letter}.channels $ch"

    v=$(printf '%d' "'$letter")
    ((v+=1))
    letter=$(printf "\x$(printf %x $v)")
done

binding=0
ttables=()
slave=0
for s in ${!slaves[@]}; do
    ch=${slaves[$s]}
    for ((i=0; i<$ch; ++i)); do
        echo "${indent}${indent}bindings.${binding}.slave $s"
        echo "${indent}${indent}bindings.${binding}.channel $i"
        ((binding++))
    done

    if [[ $ch = $inputChannels ]]; then
        gain=$(awk "BEGIN {print ($ch * $outputGain / $inputChannels)}")
    else
        gain=$(awk "BEGIN {print ($ch / ($inputChannels * $outputGain))}")
    fi
    for ((i=0; i<$inputChannels; ++i)); do
        ttables+=("${i}.$(((i % ch) + slave)) ${gain}")
    done
    ((slave+=ch))
done

echo "${indent}}"
for t in "${ttables[@]}"; do
    echo "${indent}ttable.$t"
done

echo "}"
echo ''
plug randomatones multi
plug loopin "hw:Loopback,0,0"
plug loopout "hw:Loopback,1,0"
