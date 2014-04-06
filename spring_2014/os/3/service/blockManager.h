#pragma once

#include <vector>
#include <memory>
#include <map>

#include "block.h"

class blockManager {
public:
    static blockManager& getInstance();

    std::shared_ptr<block> getBlock(int blockId);
    void formatServiceBlocks();
    std::shared_ptr<block> allocateBlock(int hint = -1);
    void deleteBlock(int blockId);
    bool canAllocate(int n);

private:
    blockManager();
    int calcServiceBlockCount();
    void loadServiceBlocks();
    int findFreeBit();
    int firstFreeBit(unsigned char num);
    void flipBit(int bitIdx);
    int countBusyBlocks();

    blockManager(blockManager const & other);
    blockManager& operator=(blockManager const & other);
    
    size_t bitsPerBlock;
    int serviceBlockCnt;
    std::vector<std::shared_ptr<block>> serviceBlocks;
    int freeBlocksCnt;

    std::map<int, std::shared_ptr<block>> cache;
};
