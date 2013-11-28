
#include "myString.h"
#include <utility>
#include <cstring>

myString::myString()
{
    length_ = 0;
    data_ = new char[1];
    data_[0] = 0;
}

myString::~myString()
{
    delete[] data_;
}

myString::myString(myString const & other)
{
    length_ = other.length_;
    data_ = new char [length_ + 1];
    
    strcpy(data_, other.data_);
}

myString::myString(char const * from)
{
    length_ = strlen(from);
    data_ = new char[length_ + 1];

    strcpy(data_, from);
}

myString& myString::operator=(myString const & other)
{
    myString(other).swap(*this);
    return *this;
}

char const * myString::c_str()
{
    return data_;
}

void myString::append(myString const & other)
{
    char * newData = new char[length_ + other.length_ + 1];
    strcpy(newData, data_);
    strcat(newData, other.data_);
    
    delete[] data_;

    data_ = newData;
    length_+=other.length_;
}

void myString::swap(myString & other)
{
    std::swap(data_, other.data_);
    std::swap(length_, other.length_);
}
