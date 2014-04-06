extern const int argsNeeded = 1;

#include <iostream>
#include <stdexcept>
#include <ctime>

#include "file.h"
#include "fileManager.h"

void printDir(std::shared_ptr<file> dir) {
    auto it = dir->getFileIterator();

    while(it.hasNext()) {
        int childId = it.next();
        auto child = std::make_shared<file>(childId);

        std::cout << child->getName() << std::endl;
    }
}

void execute(char ** argv) {
    fileManager fmanager;
    std::string path(argv[0]);

    auto curFile = fmanager.findFile(argv[0]);

    if(curFile == nullptr) {
        throw std::invalid_argument("File not found");
    }
    
    time_t rawTime = curFile->getTimeStamp();
    tm *timeinfo = localtime(&rawTime);
     
    std::cout << (path == "/" ? "/" : curFile->getName()) << std::endl;
    std::cout << "size: " << curFile->getSizeInBlocks() << std::endl;
    std::cout << "time: " << asctime(timeinfo);
    
    if(curFile->isDir()) {
        std::cout << std::endl;
        printDir(curFile);
    }
}
