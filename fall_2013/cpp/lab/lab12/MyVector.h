#ifndef MyVector_h_
#define MyVector_h_

#include <cstdlib>
#include <vector>
#include <cstdint>
#include <iostream>

template<class T>
class MyVector
{};

template<>
class MyVector<bool>
{
public:
    MyVector(size_t size=0) : size_(size)
    {}

    void push_back(bool val)
    {
        if(size_ % bitsInElement == 0)
            data_.push_back(0);
        
        set_bit(size_, val);
        ++size_;
    }
    
    void pop_back()
    {
        --size_;
        if(size_ % bitsInElement == 0)
            data_.pop_back();
    }
    
    class Bool
    {
    public:
        Bool(MyVector<bool> &vec, size_t pos)
            : vec_(vec), pos_(pos)
        {}

        Bool& operator=(bool val)
        {
            vec_.set_bit(pos_, val);
            return *this;
        }

        operator bool()
        {
            return vec_.get_bit(pos_);
        }

    private:
        MyVector<bool> &vec_;
        size_t pos_;
    };
    
    bool operator[](size_t i) const
    {
        return get_bit(i);
    }

    Bool operator[](size_t i)
    {
        return Bool(*this, i);
    }

    size_t size() const
    {
        return size_;
    }

private:
    void set_bit(size_t i, bool val)
    {
        if(!!(data_[i / bitsInElement] & (1 << (i % bitsInElement))) != val)
                data_[i / bitsInElement] ^= (1 << (i % bitsInElement));
    }

    bool get_bit(size_t i) const
    {
        return data_[i / bitsInElement] & (1 << (i % bitsInElement));
    }

    static const size_t bitsInElement = 32;
    std::vector<uint32_t> data_;
    size_t size_;
};


#endif // MyVector_h_
