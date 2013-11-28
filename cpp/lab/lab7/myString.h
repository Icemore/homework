#ifndef _myString_h_
#define _myString_h_

#include <stdlib.h>

class myString
{
    public:
    myString();
    ~myString();
    myString(myString const & other);
    myString(char const * from);
    myString& operator=(myString const & other);
    char const * c_str();
    void append(myString const & other);

    private:
    void swap(myString & other);

    char * data_;
    size_t length_;
};

class buffer
{
    public:
    buffer();
    ~buffer();

    private:
    char * data_;
};

#endif // _myString_h_
