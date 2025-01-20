device=$(~/Documents/samplescaper/singleunit/play.py | grep hdmi | grep -v hifi | cut -d' ' -f3)
echo using device $device
~/Documents/samplescaper/singleunit/play.py 3 0.3 ~/Music/pool/looped 8 1
~/Documents/samplescaper/singleunit/recycle.sh ~/Music/pool
