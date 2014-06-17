#include <boost/asio.hpp>
#include <boost/bind.hpp>

#include "fileIndex.h"
#include "consts.h"
#include <utility>
#include <memory>
#include <algorithm>
#include <thread>
#include <set> 

namespace asio = boost::asio;
using std::vector;

class sorting_task{
public:
    sorting_task(int shift, std::pair<int, int> range, fileIndex & index_, asio::io_service & io_service) 
        : shift(shift), range(range), index_(index_), io_service(io_service)
    {}

    void operator()() {
        sortOneChar();
        sortOthers();
    }
    
    void sortOthers() {
        int lastIndex = range.first;
        char lastChar = getChar(range.first);

        for(int i = range.first + 1; i < range.second; ++i) {
            char cur = getChar(i);
            
            if(cur != lastChar) {
                if(lastChar != 0) { 
                    addRange(lastIndex, i);
                }
                
                lastChar = cur;
                lastIndex = i;
            }
        }

        if(lastChar != 0) {
            addRange(lastIndex, range.second);
        }
    }

    void sortOneChar() {
        std::vector<suffix_entry> sorted(range.second - range.first);

        int cnt[CHAR_SIZE] = {};
        
        for(int i = range.first; i < range.second; ++i) {
            ++cnt[getChar(i)];
        }

        for(size_t i = 1; i < CHAR_SIZE; ++i) {
            cnt[i] += cnt[i-1];
        }

        for(int i = range.second - 1; i >= range.first; --i) {
            int dest = --cnt[getChar(i)];
            sorted[dest] = index_.suffixes[i];
        }

        std::copy(sorted.begin(), sorted.end(), index_.suffixes.begin() + range.first);
    }


private:
    void addRange(int from, int to) {
        if(to - from > 1) {
            io_service.dispatch(std::move(
                        sorting_task(shift + 1, std::make_pair(from, to), index_, io_service)));
        }
    }
    
    unsigned char getChar(int idx) {
        return getChar(index_.suffixes[idx]);
    }

    unsigned char getChar(suffix_entry & entry) {
        size_t idx = entry.suffix_idx + shift;

        file & f = index_.files[entry.file_id];

        if(idx >= f.name.size()) {
            return 0;
        }
        else {
            return f.name[idx];
        }
    }
    
    int shift;
    std::pair<int, int> range;
    fileIndex & index_;
    asio::io_service & io_service;
};

fileIndex::fileIndex(vector<file> & files) : files(files) 
{
    for(file f : files) {
        for(size_t i = 0; i < f.name.size(); ++i) {
            suffixes.push_back(suffix_entry(f.id, i));
        }
    }
    
    sort_suffixes();
}

void fileIndex::sort_suffixes() {
    //make thread pool
    asio::io_service io_service;
    std::vector<std::thread> threads(THREADS_NUMBER);
    std::unique_ptr<asio::io_service::work> work(new boost::asio::io_service::work(io_service)); 
    
    for(std::thread & thread : threads) {
        thread = std::thread(boost::bind(&asio::io_service::run, &io_service));
    }

    io_service.dispatch(std::move(sorting_task(0, std::make_pair(0, suffixes.size()), *this, io_service)));
    
    work.reset();
    for(std::thread & thread : threads) {
        thread.join();
    }
}

std::pair<int, int> fileIndex::findRange(string const & pattern) {
    int begin = lowerBound(pattern);
    int end = lowerBound(pattern + MAX_CHAR);

    return std::make_pair(begin, end);
}

int fileIndex::lowerBound(string const & pattern) {
    int begin = 0;
    int end = (int)suffixes.size() - 1;

    while(end - begin > 0) {
        int mid = (begin + end) / 2;
        
        if(getSuffix(mid) < pattern) {
            begin = mid + 1;
        }
        else {
            end = mid;
        }
    }

    if(getSuffix(end) < pattern) return suffixes.size();
    else return end;
}

std::string fileIndex::getSuffix(int idx) {
    suffix_entry & cur = suffixes[idx];
    return files[cur.file_id].name.substr(cur.suffix_idx);
}

vector<file> fileIndex::findFiles(string const & pattern) {
    std::pair<int, int> range = findRange(pattern);
    std::set<int> fileIds;

    for(int i = range.first; i < range.second; ++i) {
        fileIds.insert(suffixes[i].file_id);
    }
    
    vector<file> res;
    for(int id : fileIds) {
        res.push_back(files[id]);
    }

    return res;
}
