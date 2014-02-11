#include <iostream>
#include "BinaryReader.h"

BinaryReader::BinaryReader(std::istream &ifs) 
	: ifs_(ifs)
{
	buffer_.assign(READER_BUFFER_SIZE, 0);
	readToBuffer();
}

bool BinaryReader::readBit()
{
	bool res = (buffer_[curByte_] & (1 << (BITS_IN_BYTE - 1 - curBit_))) != 0;

	++curBit_;
	if(curBit_ == BITS_IN_BYTE)
	{
		curBit_ = 0;
		++curByte_;
		if(curByte_ == bytesInBuffer_)
			readToBuffer();
	}

	return res;
}

uint8_t BinaryReader::readByte()
{
	int bitsInNextByte = curBit_;
	uint8_t res = (buffer_[curByte_] << curBit_);

	++curByte_;
	if(curByte_ == bytesInBuffer_)
		readToBuffer();

	if(bitsInNextByte > 0)
		res |= (buffer_[curByte_] >> (BITS_IN_BYTE - bitsInNextByte));

	curBit_ = bitsInNextByte;

	return res;
}

uint64_t BinaryReader::readNumber(int bitsCount)
{
	BitChain chain;
	readBitChain(chain, bitsCount);

	uint64_t res = 0;
	for(int i = bitsCount - 1; i >= 0; --i)
	{
		res <<= 1;
		if(chain[i])
			res |= 1;
	}

	return res;
}

void BinaryReader::readBitChain(BitChain &out, int bitsCount)
{
	for(int i = 0; i < bitsCount; ++i)
		out.pushBit(readBit());
}

void BinaryReader::readVector(std::vector<uint8_t> &vec, int elmentsCount)
{
	for(int i = 0; i < elmentsCount; ++i)
		vec.push_back(readByte());
}

bool BinaryReader::eof()
{
	return bytesInBuffer_ == 0;
}

void BinaryReader::reset()
{
	ifs_.clear();
	ifs_.seekg(0, ifs_.beg);
	readToBuffer();
}

void BinaryReader::readToBuffer()
{
	buffer_[0]=0;
	ifs_.read((char*)&buffer_[0], READER_BUFFER_SIZE);
	bytesInBuffer_ = (size_t)ifs_.gcount();
	curByte_ = 0;
	curBit_ = 0;
}
