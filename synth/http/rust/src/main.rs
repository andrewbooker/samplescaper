use std::env;
use std::{
    net::TcpListener,
    io::{BufReader, BufRead, Write},
    rc::Rc
};
use regex::Regex;
use rand::Rng;


const SAMPLE_RATE: u16 = 44100;


trait ValueAt {
    fn at(&self, i: usize) -> f32;
}

struct Const {
    val: f32
}

impl ValueAt for Const {
    fn at(&self, _: usize) -> f32 {
        self.val
    }
}


struct RangeDepth {
    depth: f32,
    apply_to: Rc<dyn ValueAt>
}

impl RangeDepth {
    fn new(depth: f32, v: Rc<dyn ValueAt>) -> Self {
        RangeDepth {
            depth: depth,
            apply_to: v
        }
    }
}

impl ValueAt for RangeDepth {
    fn at(&self, i: usize) -> f32 {
        1.0 - (self.depth * 0.5 * (1.0 + self.apply_to.at(i)))
    }
}

struct RampUpDown {
    ramp_up: f32,
    ramp_down: f32,
    start_ramp_down: f32
}

impl RampUpDown {
    fn new(size: usize) -> Self {
        let fsize = size as f32;
        let mut rng = rand::rng();
        let ramp_down = fsize * rng.random_range(0.2..0.5);
        RampUpDown {
            ramp_up: SAMPLE_RATE as f32 * rng.random_range(2.0..4.0),
            ramp_down: ramp_down,
            start_ramp_down: fsize - ramp_down
        }
    }
}

impl ValueAt for RampUpDown {
    fn at(&self, ui: usize) -> f32 {
        let i = ui as f32;
        if i < self.ramp_up {
            return 0.5 * (1.0 + (std::f32::consts::PI * (i + self.ramp_up) / self.ramp_up).cos());
        }
        if i > self.start_ramp_down {
            return 0.5 * (1.0 + (std::f32::consts::PI * (i - self.start_ramp_down) / self.ramp_down).cos());
        }
        1.0
    }
}


struct Oscillator {
    freq: f32,
    phase: Rc<dyn ValueAt>,
    am: Rc<dyn ValueAt>
}

impl Oscillator {
    fn new(freq: f32, phase: Rc<dyn ValueAt>, am: Rc<dyn ValueAt>) -> Self {
        Oscillator {
            freq: freq,
            phase: phase,
            am: am
        }
    }
}

impl ValueAt for Oscillator {
    fn at(&self, i: usize) -> f32 {
        self.am.at(i) * (self.phase.at(i) + (2.0 * std::f32::consts::PI * i as f32 * self.freq / SAMPLE_RATE as f32)).sin()
    }
}


struct Synth {
    buffer: Vec<f32>,
    phase_freq: f32,
    tremolo_freq: f32,
    tremolo_depth: f32
}

trait Generator {
    fn new() -> Synth;
    fn generate(&mut self, note: u8) -> &Vec<f32>;
}


impl Generator for Synth {
    fn new() -> Synth {
        let mut rng = rand::rng();
        Synth {
            buffer: vec![0.0; (SAMPLE_RATE as f32 * rng.random_range(8.0..20.0)) as usize],
            phase_freq: rng.random_range(0.01..5.0),
            tremolo_freq: rng.random_range(0.01..6.0),
            tremolo_depth: rng.random_range(0.1..1.0)
        }
    }
    fn generate(&mut self, note: u8) -> &Vec<f32> {
        let phase_am: Rc<dyn ValueAt> = Rc::new(Const { val: 1.1 });
        let const_phase: Rc<dyn ValueAt> = Rc::new(Const { val: 0.0 });
        let phase_lfo: Rc<dyn ValueAt> = Rc::new(Oscillator::new(self.phase_freq, const_phase, phase_am));

        let zero_am_phase: Rc<dyn ValueAt> = Rc::new(Const { val: 0.0 });
        let am_level: Rc<dyn ValueAt> = Rc::new(Const { val: 1.0 });
        let am_lfo: Rc<dyn ValueAt> = Rc::new(Oscillator::new(self.tremolo_freq, zero_am_phase, am_level));
        let am: Rc<dyn ValueAt> = Rc::new(RangeDepth::new(self.tremolo_depth, am_lfo));

        let freq = f32::powf(2.0, (note as i8 - 69) as f32 / 12.0) * 440.0;

        let osc = Oscillator::new(freq, phase_lfo, am);
        let ramp = RampUpDown::new(self.buffer.len());

        for i in 0..self.buffer.len() {
            self.buffer[i] = ramp.at(i) * osc.at(i);
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
