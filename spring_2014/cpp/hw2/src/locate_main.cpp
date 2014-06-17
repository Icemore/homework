#include <iostream>
#include <fstream>
#include <string>
#include <stdexcept>

#include <boost/program_options.hpp>

#include "fileSystemWalker.h"
#include "fileIndex.h"
#include "serialization.h"

using std::string;
namespace po = boost::program_options;

void locate(string const & dbasePath, string const & pattern) {
    std::ifstream ifs(dbasePath);

    if(!ifs) {
        throw std::invalid_argument("Can't read from " + dbasePath);
    }

    boost::archive::text_iarchive ia(ifs);
    
    fileIndex d;
    ia >> d;
    
    std::vector<file> files = d.findFiles(pattern);
    for(file f : files) {
        if(fileSystemWalker::is_alive(f.path)) {
            std::cout << f.path << std::endl;
        }
    }
}

int main(int argc, char** argv) {
    string dbasePath, pattern;

    po::options_description desc;
    desc.add_options()
        ("database", po::value<string>(&dbasePath)->required(), "database file")
        ("pattern", po::value<string>(&pattern)->required(), "pattern")
    ;

    po::positional_options_description p;
    p.add("pattern", 1);
    
    try{
        po::variables_map vm;
        po::store(po::command_line_parser(argc, argv).options(desc).positional(p).run(), vm);
        po::notify(vm);

        locate(dbasePath, pattern);
    }
    catch(po::error const & err) {
        std::cerr << err.what() << std::endl << std::endl;
        std::cerr << desc << std::endl;
        return 1;
    }
    catch(std::invalid_argument const & ex) {
        std::cerr << ex.what() << std::endl;
        return 2;
    }


    return 0;
}
