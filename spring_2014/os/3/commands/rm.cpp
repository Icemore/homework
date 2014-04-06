extern const int argsNeeded = 1;

#include "fileManager.h"

void execute(char ** argv) {
    fileManager fmanager;

    fmanager.remove(argv[0]);
}
