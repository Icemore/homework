#pragma once

class block {
public:
    block(int blockId);
    ~block();
    
    void flush();
    unsigned char* getData();
    unsigned char const * getReadOnlyData() const;
    int getId();
    void clear(char val = 0);

private:
    int blockId;
    unsigned char* data;
    bool isDirty;
};
