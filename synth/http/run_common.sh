set -e
multiplexer_port=9964
curl localhost:$multiplexer_port/add=$1
trap "curl localhost:$multiplexer_port/remove=$1" EXIT
