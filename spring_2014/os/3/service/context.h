#pragma once

#include <string>

class context {
public:
    static void init(std::string rootPath_);

    static int getBlockSize();
    static int getAmountOfBlocks();
    static std::string getRootPath();
    static int getNextBlockShift();

private:
    static std::string rootPath;
    static int blockSize;
    static int amountOfBlocks;
    static int nextBlockShift;
};
