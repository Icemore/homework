#include "debug.h"

#include "blockManager.h"

#include <vector>
#include <memory>
#include <stdexcept>

#include "constants.h"
#include "context.h"
#include "block.h"

blockManager& blockManager::getInstance() {
    static blockManager instance;
    return instance;
}

std::shared_ptr<block> blockManager::getBlock(int blockId) {
    if(cache.count(blockId) == 0) {
        cache[blockId] = std::make_shared<block>(blockId); 
    }
    return cache[blockId];
}

void blockManager::formatServiceBlocks() {
    for(std::shared_ptr<block> b : serviceBlocks) {
        b->clear();
    }

    for(int i = 0; i < serviceBlockCnt; ++i){
        flipBit(i);
    }
}

std::shared_ptr<block> blockManager::allocateBlock(int hint) {
    int blockId = (hint != -1 ? hint : findFreeBit());

    if(blockId < 0)
        throw std::overflow_error("don't have any free pages");
    
    --freeBlocksCnt;

    dlog("allocating blockId =", blockId);

    flipBit(blockId);
    return getBlock(blockId);
}

void blockManager::deleteBlock(int blockId) {
    ++freeBlocksCnt;
    flipBit(blockId);
}

bool blockManager::canAllocate(int n) {
    return n <= freeBlocksCnt;
}

// private:

blockManager::blockManager() {
    bitsPerBlock = context::getBlockSize() * BITS_IN_BYTE;
    serviceBlockCnt = calcServiceBlockCount();
    loadServiceBlocks();

    freeBlocksCnt = context::getAmountOfBlocks() - countBusyBlocks();

    dlog("blockManager loaded");
    dlog("serviceBlocks:", serviceBlockCnt);
    dlog("bits per block:", bitsPerBlock);
    dlog("free blocks:", freeBlocksCnt);
    dlog();
}

static int calcNumberOfOnes(char byte) {
    int res = 0;
    while(byte) {
        ++res;
        byte &= byte - 1;
    }

    return res;
}

int blockManager::countBusyBlocks() {
    int res = 0;
    for(auto curBlock : serviceBlocks) {
        auto data = curBlock->getReadOnlyData();
        
        for(int i = 0; i < context::getBlockSize(); ++i) {
            res += calcNumberOfOnes(data[i]); 
        }
    }

    return res;
}

int blockManager::calcServiceBlockCount() {
    int needBits = context::getAmountOfBlocks();
    int needBlocks = (needBits + bitsPerBlock - 1) / bitsPerBlock;
   
    return needBlocks;
}

void blockManager::loadServiceBlocks() {
    for(int i = 0; i < serviceBlockCnt; ++i) {
        serviceBlocks.push_back(getBlock(i));
    }
}

int blockManager::findFreeBit() {
    for(int blockIdx = 0; blockIdx < serviceBlockCnt; ++blockIdx) {
        auto buf = serviceBlocks[blockIdx]->getReadOnlyData();
        
        for(int i = 0; i < context::getBlockSize(); ++i){
            auto byte = buf[i];

            if(byte != FULL_BYTE) {
                return blockIdx * bitsPerBlock + i * BITS_IN_BYTE + firstFreeBit(byte);
            }
        }
    }

    return -1;
}

int blockManager::firstFreeBit(unsigned char num) {
    int res = 0;
    while(num&1) {
        num >>= 1;
        ++res;
    }
    return res;
}

void blockManager::flipBit(int bitIdx) {
    int blockIdx = bitIdx / bitsPerBlock;
    bitIdx -= blockIdx * bitsPerBlock;
    
    int byteIdx = bitIdx / BITS_IN_BYTE;
    bitIdx -= byteIdx * BITS_IN_BYTE;

    auto data = serviceBlocks[blockIdx]->getData();
    data[byteIdx] ^= (1<<bitIdx);
}
