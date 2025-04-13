
#include <cstring>

#define bufferLength 44100
float buff[bufferLength] {};

static void interleave(float* out, const unsigned long totalLength, const unsigned int channels) {
    if (totalLength > bufferLength) {
        throw 9999;
    }
    memcpy(buff, out, totalLength * sizeof(float));
    const unsigned long partLength(totalLength / channels);
    for (unsigned long i(0); i != partLength; ++i) {
        for (unsigned int c(0); c != channels; ++c) {
            *(out + (i * channels) + c) = *(buff + i + (c * partLength));
        }
    }
}


static void interleave(float* out, const unsigned long totalLength) {
    interleave(out, totalLength, 2);
}
