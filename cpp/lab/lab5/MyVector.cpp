#include "MyVector.h"
#include <utility>

MyVector::MyVector()
{
    size = 0;
    capacity = 2;
    arr = new int [capacity];
}

MyVector::~MyVector()
{
   delete[] arr; 
}

MyVector::MyVector(MyVector const & from)
{
    size = from.size;
    capacity = from.capacity;
    arr = new int [capacity];

    for(size_t i = 0; i != size; ++i)
        arr[i] = from.arr[i];
}

void MyVector::swap(MyVector& other)
{
    std::swap(size, other.size);
    std::swap(capacity, other.capacity);
    std::swap(arr, other.arr);
}

MyVector& MyVector::operator=(MyVector const & from)
{
    MyVector(from).swap(*this);
    return *this;
}

int MyVector::Get(size_t i)
{
    return arr[i];
}

void MyVector::Set(size_t i, int val)
{
    arr[i]=val;
}

void MyVector::Add(int val)
{
    if(size == capacity)
    {
        capacity *= 2;
        int* tmp = new int[capacity];
        
        for(size_t i = 0; i != size; ++i)
            tmp[i] = arr[i];

        delete[] arr;
        arr=tmp;
    }

    arr[size++] = val;
}

size_t MyVector::GetSize()
{
    return size;
}
