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

void updatedb(string const & startPath, string const & dbasePath) {
    fileSystemWalker walker;
    std::vector<file> files = walker.findFiles(startPath);

    fileIndex d(files);

    std::ofstream ofs(dbasePath);
    
    if(!ofs) {
        throw std::invalid_argument("Can't write to " + dbasePath);
    }

    boost::archive::text_oarchive oa (ofs);

    oa << d;
}

int main(int argc, char** argv) {
    string startPath, dbasePath;

    po::options_description desc;
    desc.add_options()
        ("database-root", po::value<string>(&startPath)->required(), "directory to index")
        ("output", po::value<string>(&dbasePath)->required(), "database file name")
    ;
    
    try {
        po::variables_map vm;
        po::store(po::parse_command_line(argc, argv, desc), vm);
        po::notify(vm);
        
        updatedb(startPath, dbasePath);
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

