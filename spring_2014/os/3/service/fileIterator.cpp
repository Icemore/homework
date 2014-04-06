#include "file.h"

#include <memory>

#include "block.h"
#include "blockManager.h"
#include "constants.h"
#include "binaryHelper.h"
#include "context.h"

fileIterator::fileIterator(std::shared_ptr<block> bl) {
    curBlock = bl;
    shift = DATA_LIST_SHIFT - BYTES_IN_INT; 
}

int fileIterator::current() {
    return binaryHelper::readInt(curBlock->getReadOnlyData(), shift);
}

std::pair<std::shared_ptr<block>, int> fileIterator::position() {
    return std::make_pair(curBlock, shift);
}

bool fileIterator::hasNext() {
    int next = binaryHelper::readInt(curBlock->getReadOnlyData(),
                                     shift + BYTES_IN_INT);
    return next != -1;
}

int fileIterator::next() {
    shift += BYTES_IN_INT;
    if(shift != context::getNextBlockShift()) {
        return current();
    }

    int nextBlockId = current();
    shift = 0;
    curBlock = blockManager::getInstance().getBlock(nextBlockId);

    return current();
}
