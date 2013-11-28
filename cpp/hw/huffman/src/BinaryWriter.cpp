#include <iostream>
#include <iterator>

#include "BinaryWriter.h"

BinaryWriter::BinaryWriter(std::ostream &ofs) 
	: ofs_(ofs), currentByte_(0),  usedBitsInLastByte_(0)
{
	buffer_.assign(WRITER_BUFFER_SIZE, 0);
}

BinaryWriter::~BinaryWriter()
{
	flushBuffer();
}

void BinaryWriter::writeByte(uint8_t byte, int bitsUsed)
{
	byte &= ~((1 << (BITS_IN_BYTE - bitsUsed)) - 1);
	int bitsToCurrentByte = std::min(8 - usedBitsInLastByte_, bitsUsed);
	int bitsToNextByte = bitsUsed - bitsToCurrentByte;

	buffer_[currentByte_] |= (byte >> usedBitsInLastByte_);
	usedBitsInLastByte_ += bitsToCurrentByte;

	if(bitsToNextByte > 0)
	{
		if(currentByte_ + 1 == WRITER_BUFFER_SIZE)
			flushBuffer();
		else
			++currentByte_;

		buffer_[currentByte_] |= (byte << bitsToCurrentByte);
		usedBitsInLastByte_ = bitsToNextByte;
	}
}

void BinaryWriter::writeVector(std::vector<uint8_t> &vec)
{
	for(size_t i = 0; i < vec.size(); ++i)
		writeByte(vec[i]);
}

void BinaryWriter::writeNumber(uint64_t num, int bitsUsed)
{
	BitChain bitChain;

	for(int i = 0; i < bitsUsed; ++i)
	{
		bitChain.pushBit(num & 1);
		num >>= 1;
	}

	writeBitChain(bitChain);
}
	
void BinaryWriter::writeBitChain(BitChain &chain)
{
	std::vector<uint8_t> &data = chain.getData();

	for(size_t i = 0; i + 1 < data.size(); ++i)
		writeByte(data[i]);

	writeByte(data.back(), chain.getNumberOfBitsInLastByte());
}

void BinaryWriter::flushBuffer()
{
	if(usedBitsInLastByte_ > 0) 
		++currentByte_;

	std::ostream_iterator<uint8_t> out_iterator(ofs_);
	std::copy(buffer_.begin(), buffer_.begin()+currentByte_, out_iterator);

	std::fill(buffer_.begin(), buffer_.end(), 0);
	currentByte_ = 0;
	usedBitsInLastByte_ = 0;
}
