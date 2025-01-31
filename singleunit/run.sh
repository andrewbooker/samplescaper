device=$(~/Documents/samplescaper/singleunit/play.py | grep hdmi | cut -d' ' -f3)
echo using device $device
~/Documents/samplescaper/singleunit/play.py 3 0.3 ~/Music/pool/looped 8 5
~/Documents/samplescaper/singleunit/recycle.sh ~/Music/pool
