#include "debug.h"

extern const int argsNeeded = 1;

#include "fileManager.h"

void execute(char ** argv) {
    fileManager fmanager;

    dlog("mkdir", argv[0]);

    fmanager.mkdir(argv[0]);

    dlog("done");
}
