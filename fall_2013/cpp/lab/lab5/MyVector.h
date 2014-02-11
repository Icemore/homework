#ifndef MyVector_h_
#define MyVector_h_

#include <cstdlib>

class MyVector
{
public:
    MyVector();
    ~MyVector();
    MyVector(MyVector const & from);
    MyVector& operator=(MyVector const & from);

    int Get(size_t i);
    void Set(size_t i, int val);
    void Add(int val);
    size_t GetSize();

private:
    void swap(MyVector& from);
    size_t size;
    size_t capacity;
    int* arr;
};

#endif
