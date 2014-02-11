#include <iostream>

#include "MyVector.h"

using std::cout;
using std::endl;

int main()
{
    MyVector<bool> vec;
    
    for(size_t i = 0; i < 200; ++i)
        vec.push_back(1-i%2);
    
    bool t = vec[1];
    std::cout << t << endl; 
    
    vec[0]=false;

    for(size_t i = 0; i < vec.size(); ++i)
        std::cout << vec[i];
    std::cout << std::endl;
    
    return 0;
}
