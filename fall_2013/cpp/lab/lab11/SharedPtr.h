#ifndef SharedPtr_h_
#define SharedPtr_h_

template<class T>
class ResourceHolder
{
public:
    ResourceHolder(T* data)
        : data_(data), counter_(1)
    {}

    ~ResourceHolder()
    {
        delete data_;
    }

    void add()
    {
        ++counter_;
    }

    void release()
    {
        --counter_;
        if(counter_ == 0)
            delete this;
    }

    T* getData()
    {
        return data_;
    }

private:
    int counter_;
    T* data_;
};


template<class T>
class SharedPtr
{
public:
    explicit SharedPtr(T* data) : holder_(new ResourceHolder<T>(data))
    {}

    explicit SharedPtr(SharedPtr& other)
    {
        holder_ = other.holder_;
        holder_->add();
    }

    ~SharedPtr()
    {
        holder_->release();
    }
    
    SharedPtr& operator=(SharedPtr& other)
    {
        holder_->release();
        holder_ = other.holder_;
        holder_->add();
    }

    T& operator*()
    {
        return *holder_->getData();
    }

    T const & operator*() const
    {
        return *holder_->getData();
    }

private:
    ResourceHolder<T>* holder_;
};

#endif // SharedPtr_h_
