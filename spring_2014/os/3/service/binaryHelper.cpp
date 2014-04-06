#include "binaryHelper.h"
#include "constants.h"
#include "debug.h"

#include <cstring>

std::string binaryHelper::readString(unsigned char const * data, int offset) {
    return std::string((char*)data+offset, strlen((char*)data+offset));
}

long long binaryHelper::readNum(unsigned char const * data, int offset, int bytesCnt) {
    long long res = 0;
    for(int i = bytesCnt - 1; i >= 0; --i) {
        res <<= BITS_IN_BYTE;
        res |= data[offset + i];
    }

    return res;
}

int binaryHelper::readInt(unsigned char const * data, int offset) {
    return (int) readNum(data, offset, BYTES_IN_INT);
}

bool binaryHelper::readBool(unsigned char const * data, int offset) {
    auto res = readNum(data, offset, 1);
    return res != 0;
}

time_t binaryHelper::readTime(unsigned char const * data, int offset) {
    return readNum(data, offset, sizeof(time_t)); 
}

void binaryHelper::writeString(unsigned char * data, int offset, std::string & str) {
    for(size_t i=0; i< str.size(); ++i) {
        data[offset + i] = str[i];
    }

    // mark end of string
    data[offset + str.size()] = 0;
}

void binaryHelper::writeNum(unsigned char * data, int offset, long long num, int bytesCnt) {
    for(int i=0; i < bytesCnt; ++i) {
        data[offset + i] = num & FULL_BYTE;
        num >>= BITS_IN_BYTE;
    }
}

void binaryHelper::writeTime(unsigned char * data, int offset, time_t t) {
    writeNum(data, offset, t, sizeof(time_t));
}

void binaryHelper::writeInt(unsigned char * data, int offset, int num) {
    writeNum(data, offset, num, BYTES_IN_INT);
}

void binaryHelper::writeBool(unsigned char * data, int offset, bool val) {
    int num = val ? 1 : 0;
    writeNum(data, offset, num, 1);
}
