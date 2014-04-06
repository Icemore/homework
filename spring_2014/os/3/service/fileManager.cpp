#include "fileManager.h"

#include <stdexcept>
#include <fstream>
#include <cstring>

#include "context.h"
#include "constants.h"
#include "blockManager.h"

#include "debug.h"

fileManager::fileManager() {
    int rootBlockId = context::getAmountOfBlocks() - 1;
    rootFile = std::make_shared<file>(rootBlockId);
}

void fileManager::formatFiles() {
    int rootBlockId = context::getAmountOfBlocks() - 1;
    auto root = file::createFile("", true, 0, time(0), rootBlockId);
}

std::shared_ptr<file> fileManager::findFile(std::string strPath, parents_t & parents) {
    parents.clear();
    auto path = splitPath(strPath);

    dlog("findFile", dstr(strPath));
    dlog(path);
    dlog();

    if(path.size() == 0 || path[0] != "") {
        throw std::invalid_argument("Wrong path");
    }

    auto current = rootFile;
    std::shared_ptr<file> last = nullptr;

    
    for(size_t i = 1; i < path.size(); ++i) {
        if(current == nullptr) {
            throw std::invalid_argument("File not found");
        }

        parents.push_back(current);
        current = findChild(current, path[i]);
    }
    
    return current;
}

std::shared_ptr<file> fileManager::findFile(std::string strPath) {
    parents_t tmp;
    return findFile(strPath, tmp);
}

std::shared_ptr<file> fileManager::findChild(std::shared_ptr<file> parent, std::string const & name) {
    if(!parent->isDir()) {
        throw std::invalid_argument("Not a directory");
    }

    auto it = parent->getFileIterator();
    while(it.hasNext()) {
        int curBlockId = it.next();
        auto curFile = std::make_shared<file>(curBlockId);

        if(curFile->getName() == name) {
            return curFile;
        }
    }

    return nullptr;
}

static std::string trim(std::string const & str) {
    auto beg = str.find_first_not_of(" \t");

    if(beg == std::string::npos) {
        return "";
    }

    auto end = str.find_last_not_of(" \t");
    auto len = end - beg + 1;

    return str.substr(beg, len);
}

std::vector<std::string> fileManager::splitPath(std::string path) {
    std::string cur = "";
    cur.reserve(path.size()); // does it helps ?

    std::vector<std::string> res;

    path = trim(path);
    if(path.size() == 0) {
        return res;
    }

    if(path.back() == '/') {
        path = path.substr(0, path.size() - 1);
    }
        
    for(size_t i = 0; i < path.size(); ++i) {
        if(path[i] == '/') {
            res.push_back(cur);
            cur = "";
        }
        else {
            cur += path[i];
        }
    }

    res.push_back(cur);

    return res;
}

void fileManager::updateTimeStamps(parents_t const & files) {
    for(std::shared_ptr<file> cur : files) {
        cur->updateTimeStamp();
    }
}

std::string fileManager::getNameFromPath(std::string path) {
    std::string name = splitPath(path).back();
    
    if(name.size() > 10) {
        throw std::invalid_argument("Too long name");
    }

    return name;
}

void fileManager::move(std::string fromStrPath, std::string toStrPath) {
    parents_t fromParents, toParents;
    auto fromFile = findFile(fromStrPath, fromParents);
    auto toFile = findFile(toStrPath, toParents);

    if(toFile != nullptr) {
        throw std::invalid_argument("File already exists");
    }

    if(fromFile == nullptr) {
        throw std::invalid_argument("File not found");
    }

    std::string newName = getNameFromPath(toStrPath);

    auto fromParent = fromParents.back();
    auto toParent = toParents.back();

    fromParent->remove(fromFile->getBlockId());
    toParent->push_back(fromFile->getBlockId());

    fromFile->setName(newName);
    fromFile->updateTimeStamp();

    updateTimeStamps(fromParents);
    updateTimeStamps(toParents);
}

std::shared_ptr<file> fileManager::copyRecursively(std::shared_ptr<file> cur) {
    auto newFile = file::createFile(cur->getName(), cur->isDir(), cur->getSize());

    auto it = cur->getFileIterator();

    while(it.hasNext()) {
        int childId = it.next();

        if(cur->isDir()) {
            auto child = copyRecursively(std::make_shared<file>(childId));
            newFile->push_back(child->getBlockId());
        }
        else {
            auto dataBlock = blockManager::getInstance().getBlock(childId);
            auto newData = blockManager::getInstance().allocateBlock();

            memcpy(newData->getData(), dataBlock->getReadOnlyData(), context::getBlockSize());
            newFile->push_back(newData->getId());
        }
    }

    return newFile;
}

void fileManager::copy(std::string from, std::string to) {
    parents_t fromParents, toParents;
    auto fromFile = findFile(from, fromParents);
    auto toFile = findFile(to, toParents);
    
    std::string newName = getNameFromPath(to);
    
    if(fromFile == nullptr) {
        throw std::invalid_argument("File not found");
    }

    if(toFile != nullptr) {
        if(toFile->isDir()) {
            toParents.push_back(toFile);
            toFile = nullptr;
            newName = fromFile->getName();
        }
        else {
            throw std::invalid_argument("File already exists");
        }
    }

    int needSpace = getReadlSizeRecursively(fromFile);
    if(!blockManager::getInstance().canAllocate(needSpace+1)) {
        throw std::overflow_error("Not enough space");
    }
    
    toFile = copyRecursively(fromFile);
    toFile->setName(newName);

    toParents.back()->push_back(toFile->getBlockId());
    updateTimeStamps(toParents);
}

static int getRealFileSize(std::string path) {
    std::ifstream ifs(path);

    ifs.seekg(0, ifs.end);
    int lastPos = ifs.tellg();
    return lastPos;
}

static int calcBlocksForFile(int size) {
    int needDataBlocks = (size + context::getBlockSize() - 1) / context::getBlockSize();
    int needMetaBlocks = (needDataBlocks*BYTES_IN_INT + context::getBlockSize() - BYTES_IN_INT - 1) / (context::getBlockSize() - BYTES_IN_INT) + 1; 

    return needDataBlocks + needMetaBlocks;
}

void fileManager::readToFile(std::string path, std::shared_ptr<file> cur) {
    std::ifstream ifs(path);

    while(ifs) {
        auto dataBlock = blockManager::getInstance().allocateBlock();
        ifs.read((char*)dataBlock->getData(), context::getBlockSize());

        cur->push_back(dataBlock->getId());
    }
}

void fileManager::importFile(std::string from, std::string to) {
    parents_t parents;
    auto toFile = findFile(to, parents);
    
    if(toFile != nullptr) {
        throw std::invalid_argument("File already exists");
    }

    std::string name = getNameFromPath(to);
    int fileSize = getRealFileSize(from);
    int needBlocks = calcBlocksForFile(fileSize); 
    
    dlog("imporing file", from);
    dlog("\tsize", fileSize);
    dlog("\tneedBlocks", needBlocks);

    if(!blockManager::getInstance().canAllocate(needBlocks)) {
        throw std::overflow_error("Not enough space");
    }
    
    toFile = file::createFile(name, false, fileSize);
    readToFile(from, toFile);

    parents.back()->push_back(toFile->getBlockId());    
    updateTimeStamps(parents);
}

void fileManager::mkdir(std::string path) {
    parents_t parents;
    auto dir = findFile(path, parents);
    std::string name = getNameFromPath(path);

    if(dir != nullptr) {
        throw std::invalid_argument("File already exists");
    }

    dir = file::createFile(name, true, 0);
    
    parents.back()->push_back(dir->getBlockId());

    updateTimeStamps(parents);
}

int fileManager::getReadlSizeRecursively(std::shared_ptr<file> cur) {
    int res = cur->getNumberOfMetaBlocks();
    auto it = cur->getFileIterator();

    while(it.hasNext()) {
        int childId = it.next();

        if(cur->isDir()) {
            res += getReadlSizeRecursively(std::make_shared<file>(childId));
        }
        else {
            ++res;
        }
    }
    
    return res;
}


void fileManager::removeRecursively(std::shared_ptr<file> cur) {
    auto it = cur->getFileIterator();
    
    while(it.hasNext()) {
        int childId = it.next();

        if(cur->isDir()) {
            removeRecursively(std::make_shared<file>(childId));
        }
        else {
            blockManager::getInstance().deleteBlock(childId); 
        }
    }

    cur->wipeMetaBlocks();
}

void fileManager::remove(std::string path) {
    parents_t parents;
    auto fileToRemove = findFile(path, parents);
    
    if(fileToRemove == nullptr) {
        throw std::invalid_argument("File not found");
    }

    parents.back()->remove(fileToRemove->getBlockId());
    removeRecursively(fileToRemove);

    updateTimeStamps(parents);
}


void fileManager::exportFile(std::string fromPath, std::string toPath) {
    auto from = findFile(fromPath);
    
    if(from == nullptr) {
        throw std::invalid_argument("File not found");
    }

    std::ofstream ofs(toPath);
    
    auto it = from->getFileIterator();
    int written = 0;
    int needToWrite = from->getSize();

    dlog("exportFile");
    dlog("need to write", needToWrite);

    while(it.hasNext()) {
        int dataBlockId = it.next();
        auto dataBlock = blockManager::getInstance().getBlock(dataBlockId);

        int toWrite = std::min(context::getBlockSize(), needToWrite - written);
        
        dlog("reading from", dataBlockId);
        dlog("will write", toWrite);

        ofs.write((char*)dataBlock->getReadOnlyData(), toWrite);
        written += toWrite;
    }

    dlog();
}
