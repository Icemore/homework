#ifndef ScopedPtr_h_
#define ScopedPtr_h_

template<class T>
class ScopedPtr
{
public:
    ScopedPtr(T* data) : data_(data)
    {}

    ScopedPtr()
    {
        data_ = new T();
    }

    ~ScopedPtr()
    {
        delete data_;
    }

    ScopedPtr & operator=(T* data)
    {
        delete data_;
        data_ = data;

        return *this;
    }

    bool operator<(ScopedPtr const & other)
    {
        return data_ < other.data;
    }

    T const & operator*() const
    {
        return *data_;
    }

    T& operator*()
    {
        return *data_;
    }

    T const * operator->() const
    {
        return data_;
    }

    T* operator->()
    {
        return data_;
    }

private:
    T* data_;
};

#endif // ScopedPtr_h_
