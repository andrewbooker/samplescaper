port=$1
echo "Listening on port $port"


body() {
    awk 'BEGIN {print 0.99887766}' | od -t f4
}

respond() {
    r=$(body)
    h="Content-Type: application/octet-stream\r\nContent-Length: ${#r}\r\n"
    echo -e "HTTP/1.1 200 OK\r\n$h\r\n$r"
}

while true; do
    respond | nc -l $1 | grep GET
done

