#pragma once

#include <memory>
#include <string>
#include <vector>

#include "file.h"

typedef std::vector<std::shared_ptr<file> > parents_t;

class fileManager {
public:
    fileManager();
    static void formatFiles();
    std::shared_ptr<file> findFile(std::string strPath, parents_t & parents);
    std::shared_ptr<file> findFile(std::string strPath);

    void move(std::string from, std::string to);
    void mkdir(std::string path);
    void remove(std::string path);
    void exportFile(std::string from, std::string to);
    void copy(std::string from, std::string to);
    void importFile(std::string from, std::string to);

private:
    std::shared_ptr<file> findChild(std::shared_ptr<file> parent, std::string const & name);
    std::vector<std::string> splitPath(std::string path);
    void updateTimeStamps(parents_t const & files);
    std::string getNameFromPath(std::string path);
    void removeRecursively(std::shared_ptr<file> cur);
    int getReadlSizeRecursively(std::shared_ptr<file> cur);
    std::shared_ptr<file> copyRecursively(std::shared_ptr<file> cur);
    void readToFile(std::string path, std::shared_ptr<file> cur);

    std::shared_ptr<file> rootFile;
};
