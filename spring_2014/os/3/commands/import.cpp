extern const int argsNeeded = 2;

#include "fileManager.h"

void execute(char ** argv) {
    fileManager fmanager;

    fmanager.importFile(argv[0], argv[1]);
}
