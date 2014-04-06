#include <iostream>
#include <vector>
#include <string>

inline void dlog() {
#ifdef DEBUG
    std::cerr << std::endl;
#endif
}

template<class T> 
void print(const T & val) {
    std::cerr << val << " ";
}

template<class T>
void print(std::vector<T> const & vec) {
    std::cerr << "{";
    for(size_t i = 0; i < vec.size(); ++i) {
        if(i!=0) {
            std::cerr << ", ";
        }

        std::cerr << vec[i]; 
    }
    std::cerr << "}";
}

inline std::string dstr(std::string const & val) {
    return "\"" + val + "\"";
}

template<class Arg1, class... Args>
void dlog(const Arg1 & arg1, const Args&... args) {
#ifdef DEBUG
    print(arg1);
    dlog(args...);
#endif
}

