#include <fstream>
#include <utility>

class MyTest
{
    public:
    MyTest(int x, int y) : x(x), y(y)
    {}

    int x, y;
};

class MyOfstream : public std::ofstream
{
    public:
    MyOfstream(char const * data) 
        : std::ofstream(data)
    {}


    MyOfstream& operator<< (MyTest const & t)
    {
        std::ofstream &s = *this;
        s<<t.x<<" "<<t.y;
        return *this;
    }
};
