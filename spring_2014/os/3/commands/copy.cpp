extern const int argsNeeded = 2;

#include "fileManager.h"

void execute(char ** argv) {
    fileManager fmanager;

    fmanager.copy(argv[0], argv[1]);
}
