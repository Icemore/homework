#include "block.h"

#include <fstream>
#include <algorithm>

#include "context.h"

block::block(int blockId) 
    : blockId(blockId), isDirty(false)
{
    data = new unsigned char[context::getBlockSize()];

    std::ifstream ifs(context::getRootPath() + std::to_string(blockId),
            std::ios::binary);
    ifs.read((char*)&data[0], context::getBlockSize()); 
}

block::~block() {
    flush();
    delete[] data;
}

void block::flush() {
    if(!isDirty) return;

    std::ofstream ofs(context::getRootPath() + std::to_string(blockId),
            std::ios::binary);
    ofs.write((char*)&data[0], context::getBlockSize());
}

unsigned char* block::getData() {
    isDirty = true;
    return data;
}

unsigned char const * block::getReadOnlyData() const {
    return data;
}

int block::getId() {
    return blockId;
}

void block::clear(char val) {
    isDirty = true;
    std::fill(data, data + context::getBlockSize(), val); 
}
