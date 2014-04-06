#include <fstream>
#include <string>

#include "context.h"

extern const int argsNeeded = 0;

void execute(char ** argv) {
    for(int i = 0; i < context::getAmountOfBlocks(); ++i) {
        std::string fileName = context::getRootPath() + "/" + std::to_string(i);
        
        std::ofstream ofs(fileName, std::ios::binary);
        ofs.seekp(context::getBlockSize()-1);
        ofs.write("", 1);
    }
}
