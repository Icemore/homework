#pragma once

#include <vector>
#include "Constants.h"

class BitChain
{
public:
	BitChain();

	void pushBit(bool bit);
	void popBit();

	bool operator[](size_t index);

	size_t size();
	int numberOfOnes();

	std::vector<uint8_t>& getData();
	int getNumberOfBitsInLastByte();

private:
	std::vector<uint8_t> data_;
	int bitsUsedInLastByte_;
};