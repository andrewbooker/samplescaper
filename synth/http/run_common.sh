set -E
port=9964
host=localhost
url=$host:$port

register_port() {
    nc -z $host $port && curl $url?add=$1
}

deregister_port() {
    nc -z $host $port && curl $url?remove=$1
}

register_port $1
trap "deregister_port $1" EXIT
