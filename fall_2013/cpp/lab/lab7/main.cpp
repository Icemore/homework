#include <cstdio>
#include "myString.h"

int main()
{
    myString s("one");
    myString two("qwerty");

    s.append(two);
    s.append("wer");

    printf("%s\n", s.c_str());
    
    return 0;
}
