#include "BitChain.h"

BitChain::BitChain() 
	: bitsUsedInLastByte_(0)
{}

void BitChain::pushBit(bool bit)
{
	if(data_.empty() || bitsUsedInLastByte_ == BITS_IN_BYTE)
	{
		data_.push_back(0);
		bitsUsedInLastByte_ = 0;
	}

	if(bit)
		data_.back() |= (1 << (BITS_IN_BYTE - 1 - bitsUsedInLastByte_));

	++bitsUsedInLastByte_;
}

void BitChain::popBit()
{
	if(data_.back() & (1<<(BITS_IN_BYTE - bitsUsedInLastByte_)))
		data_.back() ^= (1<<(BITS_IN_BYTE - bitsUsedInLastByte_));

	--bitsUsedInLastByte_;

	if(bitsUsedInLastByte_ == 0 && !data_.empty())
	{
		data_.pop_back();
		bitsUsedInLastByte_ = data_.empty() ? 0 : BITS_IN_BYTE;
	}
}

bool BitChain::operator[](size_t index)
{
	return (data_[index / BITS_IN_BYTE] & (1 << (BITS_IN_BYTE - 1 - index % BITS_IN_BYTE))) > 0;
}

size_t BitChain::size()
{
	size_t res = bitsUsedInLastByte_;
		
	if(data_.size() > 1)
		res += (data_.size() - 1) * BITS_IN_BYTE;

	return res;
}

int BitChain::numberOfOnes()
{
	int result = 0;

	for(size_t i = 0; i < size(); ++i)
		if((*this)[i])
			++result;

	return result;
}

std::vector<uint8_t>& BitChain::getData()
{
	return data_;
}

int BitChain::getNumberOfBitsInLastByte()
{
	return bitsUsedInLastByte_;
}
