#include "file.h"

#include <stdexcept>

#include "blockManager.h"
#include "constants.h"
#include "context.h"
#include "binaryHelper.h"

#include "debug.h"

file::file(int blockId) : file(blockManager::getInstance().getBlock(blockId))
{}

file::file(std::shared_ptr<block> fileBlock_) {
    fileBlock = fileBlock_;
    name = binaryHelper::readString(fileBlock->getReadOnlyData(), NAME_SHIFT);
    isDir_ = binaryHelper::readBool(fileBlock->getReadOnlyData(), TYPE_SHIFT);
    lastModified = binaryHelper::readTime(fileBlock->getReadOnlyData(), TIME_SHIFT);
    sizeInBytes = binaryHelper::readInt(fileBlock->getReadOnlyData(), SIZE_SHIFT);
}

std::shared_ptr<file> file::createFile(std::string name, bool isDir, int size, time_t lastModified, int blockAllocHint) {
    auto fileBlock = blockManager::getInstance().allocateBlock(blockAllocHint);
    fileBlock->clear(-1);
    
    auto data = fileBlock->getData();
    binaryHelper::writeString(data, NAME_SHIFT, name);
    binaryHelper::writeBool(data, TYPE_SHIFT, isDir);
    binaryHelper::writeTime(data, TIME_SHIFT, lastModified);
    binaryHelper::writeInt(data, SIZE_SHIFT, size);

    dlog("create file");
    dlog("\tname:", dstr(name));
    dlog("\tisDir:", isDir);
    dlog("\tsize:", size);
    dlog("\ttime:", lastModified);
    dlog("\ton block:", fileBlock->getId());

    return std::make_shared<file>(fileBlock);
}

std::string file::getName() {
    return name;
}

time_t file::getTimeStamp() {
    return lastModified;
}

int file::getSize() {
    return sizeInBytes;
}

bool file::isDir() {
    return isDir_;
}

int file::getBlockId() {
    return fileBlock->getId();
}

void file::setName(std::string newName) {
    name = newName;
    binaryHelper::writeString(fileBlock->getData(), NAME_SHIFT, name);
}

void file::updateTimeStamp() {
    lastModified = time(0);
    binaryHelper::writeTime(fileBlock->getData(), TIME_SHIFT, lastModified);
}

void file::setSize(int newSize) {
    sizeInBytes = newSize;
    binaryHelper::writeInt(fileBlock->getData(), SIZE_SHIFT, sizeInBytes);
}

fileIterator file::getFileIterator() {
    return fileIterator(fileBlock);
}

void file::push_back(int blockId) {
    auto lastSpot = getLast();
    auto curBlock = lastSpot.first;
    auto shift = lastSpot.second;

    if(shift == context::getNextBlockShift()) {
        auto nextBlock = blockManager::getInstance().allocateBlock();
        binaryHelper::writeInt(curBlock->getData(), context::getNextBlockShift(), nextBlock->getId()); 
        curBlock = nextBlock;
        
        curBlock->clear(-1);
        shift = 0;
    }

    binaryHelper::writeInt(curBlock->getData(), shift, blockId);
}

void file::remove(int blockId) {
    remove(find(blockId));
}

void file::remove(std::pair<std::shared_ptr<block>, int> position) {
    auto last = getLast();
    last.second -= BYTES_IN_INT;
    
    int lastVal = binaryHelper::readInt(last.first->getReadOnlyData(), last.second);
    binaryHelper::writeInt(position.first->getData(), position.second, lastVal);
    
    pop_back(last);
}

void file::pop_back(std::pair<std::shared_ptr<block>, int> position) {
    if(position.second != 0) {
        binaryHelper::writeInt(position.first->getData(), position.second, -1);
        return;
    }

    auto newEndBlock = lastButOneBlock();

    blockManager::getInstance().deleteBlock(position.first->getId());
    binaryHelper::writeInt(newEndBlock->getData(), context::getNextBlockShift(), -1);
}

std::shared_ptr<block> file::lastButOneBlock() {
    auto current = fileBlock;
    auto last = current;

    while(nextBlockId(current) != -1) {
        last = current;
        int next = nextBlockId(current);
        current = blockManager::getInstance().getBlock(next);
    }
    
    return last;
}

std::pair<std::shared_ptr<block>, int> file::getLast() {
    std::shared_ptr<block> current = fileBlock;
    
    while(nextBlockId(current) != -1) {
        int next = nextBlockId(current);
        current = blockManager::getInstance().getBlock(next);
    }
    
    int start = current == fileBlock ? DATA_LIST_SHIFT : 0;
    int shift = firstEmpty(current, start);

    return make_pair(current, shift);
}

void file::wipeMetaBlocks() {
    auto current = fileBlock;

    while(current != nullptr) {
        blockManager::getInstance().deleteBlock(current->getId());

        int next = nextBlockId(current);
        if(next != -1) {
            current = blockManager::getInstance().getBlock(next);
        } else {
            current = nullptr;
        }
    }
}

int file::getNumberOfMetaBlocks() {
    auto current = fileBlock;
    int res = 1;

    while(nextBlockId(current) != -1) {
        int next = nextBlockId(current);
        current = blockManager::getInstance().getBlock(next);
        ++res;
    }

    return res;
}

int file::getSizeInBlocks() {
    int dataBlocksCnt = (sizeInBytes + context::getBlockSize() - 1) / context::getBlockSize();
    int metaBlocksCnt = getNumberOfMetaBlocks();

    return dataBlocksCnt + metaBlocksCnt;
}

int file::firstEmpty(std::shared_ptr<block> b, int start) {
    int shift = start;

    while(binaryHelper::readInt(b->getReadOnlyData(), shift) != -1) {
        shift += BYTES_IN_INT;
    }

    return shift;
}

int file::nextBlockId(std::shared_ptr<block> b) {
    return binaryHelper::readInt(b->getReadOnlyData(), context::getNextBlockShift());
}

std::pair<std::shared_ptr<block>, int> file::find(int val) {
      auto it = getFileIterator();

      while(it.hasNext()) {
          if(it.next() == val){
              return it.position();
          }
      }

      throw std::invalid_argument(std::to_string(val) + " not found in " + name);
}
