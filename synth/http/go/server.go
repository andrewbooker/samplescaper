package main


import (
	"fmt"
	"math"
	"math/rand"
	"time"
	"net/http"
	"strconv"
	"unsafe"
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
    freq float32
}

func (osc SineOscillator) at(i int) float32 {
    return float32(math.Sin(2.0 * math.Pi * float64(osc.freq) * float64(i) / SampleRate))
}


type AsPositive struct {
    value ValueAt
}

func (v *AsPositive) at(i int) float32 {
    return 0.5 * (1.0 + v.value.at(i))
}


func server(w http.ResponseWriter, r *http.Request) {
    t := anywhereBetween(8.0, 20.0)
    size := int(SampleRate * t)
    buffer := make([]float32, size)

    note, _ := strconv.Atoi(r.URL.Query()["note"][0])
    osc := SineOscillator { frequencyOf(note) }
    lfo_am := AsPositive { SineOscillator { anywhereBetween(0.001, 4.9) } }

    up := anywhereBetween(2.0, 4.0) * SampleRate
    down := int(SampleRate * t * anywhereBetween(0.2, 0.5))
    ramp := RampUpDown { int(up), down, size - down }
    for i := 0; i != size; i++ {
        buffer[i] = ramp.at(i) * lfo_am.at(i) * osc.at(i)
    }
    contentLength := size * 4
    fmt.Printf("Creating sample for note %d at %fHz lasting %fs (%d samples)\n", note, osc.freq, t, cap(buffer))
    w.Header().Set("Content-Length", strconv.Itoa(contentLength))
    w.Header().Set("Content-Type", "application/octet-stream")
    w.Write(unsafe.Slice((*byte)(unsafe.Pointer(&buffer[0])), contentLength))
}


func main() {
    rand.Seed(time.Now().UnixNano())
    http.HandleFunc("/", server)
    http.ListenAndServe(":9961", nil)
}
