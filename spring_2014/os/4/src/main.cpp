#include <iostream>
#include <cstdint>
#include <algorithm>
#include <string>
using namespace std;

uint8_t *mem;
int n;
const int headerSize = 2;

pair<bool, int16_t> readBlock(int offset) {
	int16_t head = *reinterpret_cast<int16_t*>(mem + offset);

	return make_pair(head < 0, abs(head));
}

void writeBlock(int offset, int16_t size, bool used) {
	if (used) {
		size = -size;
	}

	*reinterpret_cast<int16_t*>(mem + offset) = size;
}

int getNext(int offset) {
	return offset + readBlock(offset).second + headerSize;
}

int allocateBlock(int16_t needSize) {
	for (int offset = 0; offset < n; offset = getNext(offset)) {
		auto block = readBlock(offset);
		
		if (block.first || block.second < needSize) {
			continue;
		}

		int leftover = block.second - needSize;

		if (leftover < 2 * headerSize) {
			writeBlock(offset, block.second, true);
			return offset;
		}
		else {
			leftover -= headerSize;

			writeBlock(offset, leftover, false);
			int newBlockOffset = offset + leftover + headerSize;
			writeBlock(newBlockOffset, needSize, true);
			return newBlockOffset;
		}
	}

	return -1;
}

void joinBlocks() {
	int lastOffset = 0;
	auto lastBlock = readBlock(lastOffset);

	for (int curOffset = getNext(0); curOffset < n; curOffset = getNext(curOffset)) {
		auto curBlock = readBlock(curOffset);

		if (!lastBlock.first && !curBlock.first) {
			writeBlock(lastOffset, lastBlock.second + curBlock.second + headerSize, false);

			lastBlock = readBlock(lastOffset);
		}
		else {
			lastOffset = curOffset;
			lastBlock = curBlock;
		}
	}
}

bool freeBlock(int offset) {
	for (int curOffset = 0; curOffset < n; curOffset = getNext(curOffset)) {
		if (curOffset != offset) {
			continue;
		}

		auto block = readBlock(offset);

		if (!block.first) {
			return false;
		}

		writeBlock(offset, block.second, false);
		joinBlocks();
		return true;
	}
	return false;
}

void printInfo() {
	int allocatedBlocksCnt = 0;
	int allocatedMemory = 0;
	int16_t maxFreeChunk = 0;

	for (int offset = 0; offset < n; offset = getNext(offset)) {
		auto block = readBlock(offset);

		if (block.first) {
			++allocatedBlocksCnt;
			allocatedMemory += block.second;
		}
		else {
			maxFreeChunk = max(maxFreeChunk, block.second);
		}
	}

	cout << allocatedBlocksCnt << " " << allocatedMemory << " " << maxFreeChunk << endl;
}

void printMap() {
	for (int offset = 0; offset < n; offset = getNext(offset)) {
		auto block = readBlock(offset);

		char ch = block.first ? 'u' : 'f';
		cout << string(headerSize, 'm');
		cout << string(block.second, ch);
	}
	cout << endl;
}

void init() {
	writeBlock(0, n - headerSize, false);
}

void doAllocate(){
	int16_t	size;
	cin >> size;

	int res = allocateBlock(size);
	if (res < 0) {
		cout << "-" << endl;
	}
	else {
		cout << "+ " << res << endl;
	}
}

void doFree() {
	int offset;
	cin >> offset;

	if (freeBlock(offset)) {
		cout << "+" << endl;
	}
	else{
		cout << "-" << endl;
	}
}
void run() {
	string str;

	while (cin >> str) {
		if (str == "ALLOC") {
			doAllocate();
		}
		else if (str == "FREE") {
			doFree();
		}
		else if (str == "INFO") {
			printInfo();
		}
		else if (str == "MAP") {
			printMap();
		}
	}
}

int main() {
	cin >> n;
	mem = new uint8_t[n];
	init();

	run();

	delete[] mem;
		
	return 0;
}