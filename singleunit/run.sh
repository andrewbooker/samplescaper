device=$(~/Documents/samplescaper/singleunit/play.py | sed -nE 's/.*([0-9]).+hdmi.*/\1/p')
echo using device $device
~/Documents/samplescaper/singleunit/play.py $device 0.3 ~/Music/pool/looped 8
