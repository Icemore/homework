#pragma once

#include <iostream>
#include <string>
#include <vector>

#include "BitChain.h"
#include "Constants.h"

class BinaryReader
{
public:
	BinaryReader(std::istream &ifs);

	bool readBit();
	uint8_t readByte();
	uint64_t readNumber(int bitsCount);
	void readBitChain(BitChain &out, int bitsCount);
	void readVector(std::vector<uint8_t> &vec, int elmentsCount);

	bool eof();
	void reset();

private:
	void readToBuffer();

	std::istream &ifs_;
	std::vector<uint8_t> buffer_;
	int curByte_;
	int curBit_;
	int bytesInBuffer_;
};
