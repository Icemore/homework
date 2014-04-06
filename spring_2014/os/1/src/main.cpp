#include <iostream>
#include <errno.h>
#include <cstdlib>
#include <string>
#include <vector>
#include <map>
#include <dirent.h>
#include <sstream>

#include "commands.h"

using std::string;
using std::vector;

typedef void (*cmd_function)(vector<string> const &);
static std::map<string, cmd_function> cmds = {
    {"ls", ls},
    {"kill", kill},
    {"ps", ps},
    {"pwd", pwd}
};

void split(string str, vector<string> & args) {
    args.clear();
    
    std::stringstream ss(str);
    string cur;

    while(ss >> cur) {
        args.push_back(cur);
    }
}

int main(int argc, char** argv) {
    string input_line;
    vector<string> command_args;

    while(true) {
        std::cout << "# ";
        getline(std::cin, input_line);

        split(input_line, command_args);

        if(command_args.size() < 1) 
            continue;

        if(command_args[0] == "exit")
            break;

        if(cmds.count(command_args[0]) == 0) {
            int ret = system(input_line.c_str());
            if(ret < 0) {
                std::cout << "Error(" << errno << ") invoking system" << std::endl;
            }
        }
        else {
            cmds[command_args[0]](command_args);
        }
    }

    return 0;
}
