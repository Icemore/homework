#include <iostream>
#include "MyOfstream.h"

int main()
{
    MyOfstream fout("out.txt");
    MyTest t(34, 1);

    fout<<t;

    return 0;
}
