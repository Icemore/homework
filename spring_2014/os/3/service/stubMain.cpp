#include <iostream>
#include <stdexcept>

#include "context.h"

extern const int argsNeeded;
void execute(char **argv);


int main(int argc, char **argv) {
    if(argc < argsNeeded + 2) {
        std::cerr << "Not enough arguments" << std::endl;
        return 1;
    }

    context::init(argv[1]);    

    try {
        execute(argv+2); 
    }
    catch(std::exception & ex) {
        std::cerr << "Operation failed: " << ex.what() << std::endl;
        return 2;
    }
    
    return 0;
}
