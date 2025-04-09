

static const unsigned long posFrom(const unsigned long i, const unsigned long partLength) {
    if (i < partLength) {
        return 2 * i;
    }
    return (2 * (i - partLength)) + 1;
}

static const unsigned long inversePosFrom(const unsigned long i, const unsigned long partLength) {
    if (i % 2 != 0) {
        return partLength + ((i - 1) / 2);
    }
    return i / 2;
}


static void interleaveA(float* out, const unsigned long totalLength) {
    const unsigned long partLength(totalLength / 2);

    unsigned long i(1), fetchPos(-1);
    float c(0);
    while (i != totalLength) {
        if (fetchPos > 0) {
            if (fetchPos == i) {
                *(out + i) = c;
                ++i;
            } else {
                auto f(inversePosFrom(fetchPos, partLength));
                *(out + fetchPos) = *(out + f);
                fetchPos = f;
            }
        } else {
            fetchPos = inversePosFrom(i, partLength);
            c = *(out + i);
            *(out + i) = *(out + fetchPos);
            ++i;
        }
    }    
}

static void interleave(float* out, const unsigned long totalLength) {
    float c(*(out + 1));
    *(out + 1) = *(out + 2);
    *(out + 2) = c;
}
