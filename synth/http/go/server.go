package main


import (
	"fmt"
	"math"
	"math/rand"
	"time"
	"net/http"
	"strconv"
	"unsafe"
	"flag"
)

const SampleRate = 44100

func frequencyOf(note int) float32 {
    return float32(math.Pow(2, float64(note - 69) / 12.0) * 440.0)
}

func anywhereBetween(l, u float32) float32 {
    return (rand.Float32() * (u - l)) + l
}


type ValueAt interface {
    at(i int) float32
}

type RampUpDown struct {
    up, down, startDown int
}

func (ramp RampUpDown) at(i int) float32 {
    if i < ramp.up {
        return 0.5 * float32(1.0 + math.Cos(math.Pi * float64(i + ramp.up) / float64(ramp.up)))
    }
    if i > ramp.startDown {
        return 0.5 * float32(1.0 + math.Cos(math.Pi * float64(i - ramp.startDown) / float64(ramp.down)))
    }
    return 1.0
}


type SineOscillator struct {
    freq float32;
    phase ValueAt
}

func (osc SineOscillator) at(i int) float32 {
    return float32(math.Sin(float64(osc.phase.at(i)) + (2.0 * math.Pi * float64(osc.freq) * float64(i) / SampleRate)))
}


type AsPositive struct {
    value ValueAt
}

func (v *AsPositive) at(i int) float32 {
    return 0.5 * (1.0 + v.value.at(i))
}


type RangeDepth struct {
    depth float32;
    value ValueAt
}

func (v *RangeDepth) at(i int) float32 {
    return 1.0 - (v.depth * 0.5 * (1.0 + v.value.at(i)))
}


type Scaled struct {
    coeff float32;
    value ValueAt
}

func (s *Scaled) at(i int) float32 {
    return s.coeff * s.value.at(i)
}


type Const struct {
    value float32
}

func (c *Const) at(i int) float32 {
    return c.value
}


func server(w http.ResponseWriter, r *http.Request) {
    t := anywhereBetween(8.0, 20.0)
    size := int(SampleRate * t)
    buffer := make([]float32, size)
    note, _ := strconv.Atoi(r.URL.Query()["note"][0])

    zero := Const { 0.0 }
    lfo_phase := Scaled { anywhereBetween(0.1, math.Pi), &SineOscillator { anywhereBetween(0.1, 5.8), &zero } }
    osc := SineOscillator { frequencyOf(note), &lfo_phase }
    lfo_am := RangeDepth { anywhereBetween(0.1, 1.0), &AsPositive { &SineOscillator { anywhereBetween(0.001, 4.9), &zero } } }
    up := anywhereBetween(2.0, 4.0) * SampleRate
    down := int(SampleRate * t * anywhereBetween(0.2, 0.5))
    ramp := RampUpDown { int(up), down, size - down }
    for i := 0; i != size; i++ {
        buffer[i] = ramp.at(i) * lfo_am.at(i) * osc.at(i)
    }
    contentLength := size * 4
    fmt.Printf("Go note %d at %fHz lasting %fs (%d samples)\n", note, osc.freq, t, cap(buffer))
    w.Header().Set("Content-Length", strconv.Itoa(contentLength))
    w.Header().Set("Content-Type", "application/octet-stream")
    w.Write(unsafe.Slice((*byte)(unsafe.Pointer(&buffer[0])), contentLength))
}


func main() {
    flag.Parse()
    vals := flag.Args()
    if len(vals) == 0 {
        fmt.Printf("Must supply port number\n")
        return
    }
    port, err := strconv.Atoi(vals[0])
    if err != nil {
        fmt.Printf("Invalid supply port number\n", err, "\n\n")
        return
    }
    rand.Seed(time.Now().UnixNano())
    http.HandleFunc("/", server)
    http.ListenAndServe(fmt.Sprintf(":%d", port), nil)
}
