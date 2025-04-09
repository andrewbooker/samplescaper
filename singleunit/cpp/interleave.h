#include <iostream>  // remove this

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


static void interleave(float* out, const unsigned long totalLength) {
    const unsigned long partLength(totalLength / 2);

    unsigned long i(1), done(1), fetchPos(0);
    float c(0);
    while (done != (totalLength - 1)) {
        std::cout << "done:" << done << ", i:" << i;
        if (fetchPos > 0) {
            std::cout << ", just moved:" << out[fetchPos];
        }
        std::cout << ", c=" << c << "\nbuff=";

        if (fetchPos > 0) {
            if (fetchPos == i) {
                *(out + i) = c;
                ++i;
                fetchPos = 0;
            } else {
                const unsigned long f(inversePosFrom(fetchPos, partLength));
                *(out + fetchPos) = *(out + f);
                fetchPos = f;
            }
        } else {
            fetchPos = inversePosFrom(i, partLength);
            c = *(out + i);
            *(out + i) = *(out + fetchPos);
            *(out + fetchPos) = c;
            if (i != 1) {
                break;
            }
            ++i;
        }
        ++done;
        for (int j(0); j != totalLength; ++j) {
            std::cout << *(out + j) << ",";
        }
        std::cout << "\n";
    }    
}
