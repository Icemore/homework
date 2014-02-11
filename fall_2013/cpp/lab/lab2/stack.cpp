#include <cstdio>

int  hello()
{
    printf("Hacked!\n");
    return 0;
}

int f()
{
    void* c[5];
    for(int i=0; i<20; ++i)
        c[i]=(void*)&hello;

    return 0;
}

int main()
{
    f();
    return 0;
}
