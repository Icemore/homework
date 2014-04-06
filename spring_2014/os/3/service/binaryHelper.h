#pragma once

#include <ctime>
#include <string>

class binaryHelper {
public:
    static std::string readString(unsigned char const * data, int offset);
    static long long readNum(unsigned char const * data, int offset, int bytesCnt);
    static int readInt(unsigned char const * data, int offset);
    static bool readBool(unsigned char const * data, int offset);
    static time_t readTime(unsigned char const * data, int offset);

    static void writeString(unsigned char * data, int offset, std::string & str);
    static void writeNum(unsigned char * data, int offset, long long num, int bytesCnt);
    static void writeTime(unsigned char * data, int offset, time_t t);
    static void writeInt(unsigned char * data, int offset, int num);
    static void writeBool(unsigned char * data, int offset, bool val);
};
