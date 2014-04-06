extern const int argsNeeded = 0;

#include "debug.h"

#include "fileManager.h"
#include "blockManager.h"

void execute(char ** argv) {
    blockManager::getInstance().formatServiceBlocks();
    fileManager::formatFiles();
    
    dlog("done");
}
