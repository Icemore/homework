#pragma once

#include <boost/filesystem.hpp>
#include <queue>
#include <vector>
#include <string>
#include <mutex>
#include <condition_variable>

#include "fileIndex.h"
#include "consts.h"

using boost::filesystem::path;

class fileSystemWalker {
public:
    std::vector<file> findFiles(std::string const & start);

    static bool is_alive(std::string const & fileName);

private:
    void addToQueue(std::vector<path> const & newDirs);
    void getFilePaths(path start);   
    std::vector<file> getFiles();
    void printError(path p, boost::system::error_code & err);
    void printError(path p, std::string const & msg);
    void walk_task(size_t id);
    bool processFile(path file, size_t id);
    void processDir(path dir, size_t id);
    void clearAll();

    bool done;
    size_t waitingCnt;
    std::mutex queueMutex;
    std::condition_variable cond;
    std::mutex errMutex;

    std::queue<path> fileQueue;
    std::vector<path> files[THREADS_NUMBER];
};
