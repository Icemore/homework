#pragma once

#include <vector>
#include <string>
using std::string;

struct file {
    file() {}

    file(string const & path, string const & name, int id) 
        : path(path), name(name), id(id) 
    {}
    
    string path;
    string name;
    int id;
};

struct suffix_entry {
    suffix_entry() {}

    suffix_entry(int file_id, int suffix_idx) 
        : file_id(file_id), suffix_idx(suffix_idx)
    {}

    int file_id;
    int suffix_idx;
};

class fileIndex{
public: 
    fileIndex() {}
    fileIndex(std::vector<file> & files);

    std::pair<int, int> findRange(string const & pattern);
    std::vector<file> findFiles(string const & pattern);

    std::vector<file> files;
    std::vector<suffix_entry> suffixes;
private:
    void sort_suffixes();
    std::string getSuffix(int idx);
    int lowerBound(string const & pattern);
};
