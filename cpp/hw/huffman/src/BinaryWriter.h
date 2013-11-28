#pragma once

#include <iostream>
#include <vector>

#include "BitChain.h"
#include "Constants.h"

class BinaryWriter
{
public:
	BinaryWriter(std::ostream &ofs);
	~BinaryWriter();

	void writeByte(uint8_t byte, int bitsUsed = BITS_IN_BYTE);
	void writeVector(std::vector<uint8_t> &vec);
	void writeNumber(uint64_t num, int bitsUsed);
	void writeBitChain(BitChain &chain);
	void flushBuffer();

private:
	std::ostream &ofs_;
	std::vector<uint8_t> buffer_;
	int currentByte_;
	int usedBitsInLastByte_;
};
