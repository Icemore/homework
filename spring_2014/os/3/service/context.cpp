#include "context.h"

#include <fstream>
#include "constants.h"

void context::init(std::string rootPath_) {
    rootPath = rootPath_;
    if(rootPath.back() != '/')
        rootPath.push_back('/');

    std::ifstream ifs(rootPath + "config");

    ifs >> blockSize >> amountOfBlocks;
    nextBlockShift = blockSize - BYTES_IN_INT;
}

int context::getBlockSize() {
    return blockSize;
}

int context::getAmountOfBlocks() {
    return amountOfBlocks;
}

std::string context::getRootPath() {
    return rootPath;
}

int context::getNextBlockShift() {
    return nextBlockShift;
}

std::string context::rootPath;
int context::blockSize;
int context::amountOfBlocks;
int context::nextBlockShift;
