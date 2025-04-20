use std::env;
use std::{
    net::TcpListener,
    io::{BufReader, BufRead, Write}
};
use regex::Regex;


fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        println!("Must support port number");
        return;
    }

    let port = &args[1];
    let host = format!("0.0.0.0:{port}");
    let listener = TcpListener::bind(host).unwrap();
    println!("tcp listener started on port {}", port);

    for stream in listener.incoming() {
        let mut stream = stream.unwrap();
        let buf_reader = BufReader::new(&mut stream);
        let http_request: Vec<_> = buf_reader
            .lines()
            .map(|result| result.unwrap())
            .take_while(|line| !line.is_empty())
            .collect();

        let re = Regex::new(r"^([A-Z]+) /[\?]?([a-z]+)=?([0-9]*)").unwrap();
        for (_, [method, param, value]) in re.captures_iter(&http_request[0]).map(|c| c.extract()) {
            println!("{method} {param} {value}");
        }

        let status_line = "HTTP/1.1 200 OK";
        let length = 0;
        let response = format!("{status_line}\r\nContent-Type: application/octet-stream\r\nContent-Length: {length}\r\n\r\n");
        stream.write_all(response.as_bytes()).unwrap();
    }
}
