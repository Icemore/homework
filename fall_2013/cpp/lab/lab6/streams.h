#include <cstdio>
#include <string>

class IStream
{
    public:
    virtual void write(int value)=0;
    virtual void write(std::string const & value)=0;
    virtual ~IStream(){}
};

class FileStream : public IStream
{
    public:
    FileStream(std::string const & path)
    {
        f=fopen(path.c_str(), "w");
    }

    ~FileStream()
    {
        fclose(f);
    }

    virtual void write(int value)
    {
        fprintf(f, "%d", value);
    }

    virtual void write(std::string const & value)
    {
        fprintf(f, "%s", value.c_str());
    }
    
    private:
    FileStream(FileStream const & other);
    FileStream& operator=(FileStream const & other);
    FILE* f;
};

class PPStream : public IStream
{
    public:
    PPStream(IStream & stream) : stream_(stream), linesCnt_(1)
    {}

    virtual void write(int value)
    {
        stream_.write(linesCnt_++);
        stream_.write(": ");
        stream_.write(value);
        stream_.write("\n");
    }
    
    virtual void write(std::string const & value)
    {
        stream_.write(linesCnt_++);
        stream_.write(": ");
        stream_.write(value);
        stream_.write("\n");
    }

    private:
    IStream& stream_;
    int linesCnt_;
};

class ConsoleWriter : public IStream
{
    public:
    ConsoleWriter(IStream & stream) : stream_(stream)
    {}

    virtual void write(int value)
    {
        printf("%d", value);
        stream_.write(value);
    }

    virtual void write(std::string const & value)
    {
        printf("%s", value.c_str());
        stream_.write(value);
    }

    private:
    IStream& stream_;
};
