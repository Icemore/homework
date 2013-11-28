#include <iostream>
#include <utility>
#include "SharedPtr.h"

using namespace std;

int main()
{

    int *p = new int(5);
    {
        SharedPtr<int> shp(p);
       
        cout<<(*shp)<<endl;
        {
            SharedPtr<int> shp2(shp);
            cout<<(*shp2)<<endl;
        }
        cout<<(*shp)<<endl;
    }
    cout<<(*p)<<endl;

    return 0;
}
