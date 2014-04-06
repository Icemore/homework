#pragma once

#include <string>
#include <memory>

#include "block.h"

class fileIterator;

class file {
public:
    file(int blockId);
    file(std::shared_ptr<block> fileBlock_);
    static std::shared_ptr<file> createFile(std::string name, bool isDir, int size, time_t lastModified = time(0), int blockAllocHint = -1);

    std::string getName();
    time_t getTimeStamp();
    int getSize();
    bool isDir();
    int getBlockId();
    int getSizeInBlocks();

    void setName(std::string newName);
    void updateTimeStamp();
    void setSize(int newSize);

    fileIterator getFileIterator();
    void push_back(int blockId);

    void remove(int blockId);
    void remove(std::pair<std::shared_ptr<block>, int> position);
    
    void wipeMetaBlocks();
    int getNumberOfMetaBlocks(); 
private:
    void pop_back(std::pair<std::shared_ptr<block>, int> position);
    std::shared_ptr<block> lastButOneBlock();
    std::pair<std::shared_ptr<block>, int> getLast();
    int firstEmpty(std::shared_ptr<block> b, int start);
    int nextBlockId(std::shared_ptr<block> b);
    std::pair<std::shared_ptr<block>, int> find(int val);


    std::shared_ptr<block> fileBlock;
    std::string name;
    bool isDir_;
    time_t lastModified;
    int sizeInBytes;
};

class fileIterator
{
public:
    fileIterator(std::shared_ptr<block> bl);
    
    int current();
    std::pair<std::shared_ptr<block>, int> position();
    bool hasNext();
    int next();

private:
    std::shared_ptr<block> curBlock;
    int shift;
};

