use std::env;
use std::{
    net::TcpListener,
    io::{BufReader, BufRead, Write}
};
use regex::Regex;
use rand::Rng;


const SAMPLE_RATE: u16 = 44100;


struct Synth {
    buffer: Vec<f32>
}

trait Generator {
    fn new() -> Synth;
    fn generate(&mut self, note: u8) -> &Vec<f32>;
}


impl Generator for Synth {
    fn new() -> Synth {
        let mut rng = rand::rng();
        Synth {
            buffer: vec![0.0; (SAMPLE_RATE as f32 * rng.random_range(8.0..20.0)) as usize]
        }
    }
    fn generate(&mut self, note: u8) -> &Vec<f32> {
        let freq = f32::powf(2.0, (note as i8 - 69) as f32 / 12.0) * 440.0;
        let ang_freq = freq * 2.0 * std::f32::consts::PI;
        for i in 0..self.buffer.len() {
            self.buffer[i] = (i as f32 * ang_freq / SAMPLE_RATE as f32).sin();
        }
        &self.buffer
    }
}


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

            let mut synth = Synth::new();
            let note = value.parse::<u8>().unwrap();
            let synth_buffer = synth.generate(note);

            let status_line = "HTTP/1.1 200 OK";
            let length = synth_buffer.len() * std::mem::size_of::<f32>();
            let response = format!("{status_line}\r\nContent-Type: application/octet-stream\r\nContent-Length: {length}\r\n\r\n");
            stream.write_all(response.as_bytes()).unwrap();

            unsafe { stream.write_all(std::slice::from_raw_parts(synth_buffer.as_ptr() as *const u8, length)).unwrap() }
        }
    }
}
