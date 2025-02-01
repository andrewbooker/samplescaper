script_loc=~/Documents/samplescaper/singleunit
echo $(date) > /var/log/samplescaper/player.log
$script_loc/fill_pool.sh &
$script_loc/play.py 1 0.3 ~/Music/pool/looped 8 2> /var/log/samplescaper/player_error.log
