#include <iostream>
#include <iomanip>
#include <fstream>
#include <errno.h>
#include <string.h>
#include <signal.h>
#include <string>
#include <vector>
#include <algorithm>
#include <sys/types.h>
#include <dirent.h>
#include <unistd.h>

void listdir(std::string dir, std::vector<std::string> & res) {
    DIR * dp = opendir(dir.c_str());

    if(dp == NULL) {
        std::cout << "Error(" << errno << ") opening " << dir << std::endl;
        return;
    }
    
    res.clear();
    struct dirent *dirp;
    while((dirp = readdir(dp)) != NULL) {
        res.push_back(dirp->d_name);
    }

    closedir(dp);
}

bool is_number(std::string const & str) {
    return !str.empty() && std::all_of(str.begin(), str.end(), ::isdigit);
}

void ls(std::vector<std::string> const & args) {
    std::vector<std::string> list;
    listdir(".", list);

    for(size_t i = 0; i < list.size(); ++i) {
        std::cout << list[i] << std::endl;
    }
}

void kill(std::vector<std::string> const & args){
   if(args.size() != 3 ||
      !is_number(args[1].substr(1)) ||
      !is_number(args[2])) 
   {
       std::cout << "Wrong arguments" << std::endl;
       std::cout << "kill -<signal> <pid>" << std::endl;
       return;
   }

   int pid = std::stoi(args[2]);
   int signal = std::stoi(args[1].substr(1));
    
   if(kill(pid, signal) < 0) {
        std::cout << "Error(" << errno << "): " << strerror(errno) << std::endl;
   }
}

void pwd(std::vector<std::string> const & args){
    char buf[1024];

    if(getcwd(buf, sizeof(buf)) == NULL) {
        std::cout << "Error(" << errno << ") performing pwd" << std::endl;
        return;
    }

    std::cout << buf << std::endl;
}

void ps(std::vector<std::string> const & args){
    std::vector<std::string> procs;

    listdir("/proc", procs);
 
    std::cout << std::setw(5) << "PID" << " ";
    std::cout << "NAME" << std::endl;
    for(size_t i = 0; i < procs.size(); ++i) {
        if(!is_number(procs[i])) 
            continue;
        
        std::string name;
        std::ifstream ifs("/proc/" + procs[i] + "/status");
        getline(ifs, name);
        name = name.substr(6);

        std::cout << std::setw(5) << procs[i] << " " << name << std::endl;
    }
}
