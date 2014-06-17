#include <thread>
#include <mutex>
#include <condition_variable>
#include <iostream>
#include <functional>
#include <stdexcept>

#include "fileSystemWalker.h"
#include "consts.h"

namespace fs = boost::filesystem;
using fs::path;
using boost::system::error_code;


void fileSystemWalker::addToQueue(std::vector<path> const & newDirs) {
    std::unique_lock<std::mutex> ulock(queueMutex);
    
    for(path dir : newDirs) {
        fileQueue.push(dir);
    }

    cond.notify_all();
}

bool fileSystemWalker::processFile(path file, size_t id) {
    bool isDir = false;

    try {
        path cur = fs::canonical(file);

        if(!fs::is_symlink(file) && fs::is_directory(file)) {
            isDir = true;
        }

        files[id].push_back(cur);
    }
    catch(const fs::filesystem_error & ex) {
        printError(file, ex.what());
        isDir = false;
    }

    return isDir;
}

void fileSystemWalker::clearAll() {
    for(size_t i = 0; i < THREADS_NUMBER; ++i){
        files[i].clear();
    }

    while(!fileQueue.empty()) {
        fileQueue.pop();
    }

    done = false;
    waitingCnt = 0;
}

void fileSystemWalker::getFilePaths(path start) {
    clearAll();
    
    if(processFile(start, 0)) {
        addToQueue({start});
    }
    else {
        throw std::invalid_argument(start.string() + " is not a directory");
    }
    
    std::thread threads[THREADS_NUMBER];
    for(size_t i = 0; i < THREADS_NUMBER; ++i) {
        threads[i] = std::thread(std::bind(&fileSystemWalker::walk_task, this, i));
    }

    for(size_t i = 0; i < THREADS_NUMBER; ++i) {
        threads[i].join();
    }
}

std::vector<file> fileSystemWalker::getFiles() {
    std::vector<file> res;
    int id = 0;

    for(size_t i = 0; i < THREADS_NUMBER; ++i) {
        for(path p : files[i]) {
            file f(p.string(), p.filename().string(), id++);
            res.push_back(f);
        }
    }

    return res;
}

std::vector<file> fileSystemWalker::findFiles(std::string const & start) {
    getFilePaths(start);
    return getFiles();
}

void fileSystemWalker::walk_task(size_t id) {
    while(!done) {
        std::unique_lock<std::mutex> ulock(queueMutex);

        while(!done && fileQueue.empty()) {
            ++waitingCnt;
            if(waitingCnt == THREADS_NUMBER) break;
            cond.wait(ulock);
            --waitingCnt;
        }
        
        if(fileQueue.empty()) {
            done = true;
            cond.notify_all();
            break;
        }

        path cur = fileQueue.front();
        fileQueue.pop();

        ulock.unlock();
        
        processDir(cur, id);
    }
}

void fileSystemWalker::processDir(path dir, size_t id) {
    std::vector<path> newDirs;
    error_code err;

    for(auto it = fs::directory_iterator(dir, err); !err && it != fs::directory_iterator(); it.increment(err)) {
        path cur = it->path();

        if(processFile(cur, id)) {
            newDirs.push_back(cur);
        }
    }

    if(err) {
        printError(dir, err);
        return;
    }

    addToQueue(newDirs);
}



void fileSystemWalker::printError(path p, error_code & err) {
    std::unique_lock<std::mutex> errLock(errMutex);

    std::cerr << p << ": " << err.message() << std::endl;
}

void fileSystemWalker::printError(path p, std::string const & msg) {
    std::unique_lock<std::mutex> errLock(errMutex);

    std::cerr << msg << std::endl;
}

bool fileSystemWalker::is_alive(std::string const & fileName) {
    error_code err;

    return (fs::exists(fileName, err) && !err);
}
