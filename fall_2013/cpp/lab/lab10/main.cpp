#include <iostream>

#include "Rational.h"

int main()
{
    Rational r;
    
    std::cin >> r;
    std::cout << r++ << std::endl;
    std::cout << r;
    return 0;

    Rational r1 = 5;
    Rational r2 = 15;
    
    std::cout << r1 / r2 << std::endl;
    
    Rational t = (r1 * r2) / r1;

    std::cout << (double)t << std::endl;

    return 0;
}
