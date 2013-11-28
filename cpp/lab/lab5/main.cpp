#include <iostream>
#include "MyVector.h"

using namespace std;

int main()
{
    MyVector t2;

    {
        MyVector t1;
        for(size_t i = 0; i != 20; ++i)
            t1.Add(i);
        t2 = t1;
    }

    for(size_t i = 0; i != t2.GetSize(); ++i)
        cout << t2.Get(i) << " ";
    cout << endl;

    return 0;
}
