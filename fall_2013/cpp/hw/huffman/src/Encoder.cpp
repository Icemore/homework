#include <vector>
#include <queue>
#include <iostream>

#include "BinaryWriter.h"
#include "BinaryReader.h"
#include "BitChain.h"
#include "HuffmanTree.h"

static void encodeElements(node* v, BitChain& bits, std::vector<BitChain> &encodedElements)
{
	if(v->isElementNode)
	{
		encodedElements[v->element] = bits;
	}
	
	if(v->left)
	{
		bits.pushBit(0);
		encodeElements(v->left, bits, encodedElements);
		bits.popBit();
	}

	if(v->right)
	{
		bits.pushBit(1);
		encodeElements(v->right, bits, encodedElements);
		bits.popBit();
	}
}

static uint64_t countElements(BinaryReader &reader, std::vector<uint64_t> &elementsCount)
{
	uint64_t elementsInFile = 0;
	elementsCount.assign(ELEMENTS_NUMBER, 0);

	while(!reader.eof())
	{
		++elementsCount[reader.readByte()];
		++elementsInFile;
	}

	return elementsInFile;
}

static void writeMetaInfo(node* root, uint64_t elementsInFile, BinaryWriter &writer)
{
	BitChain treeInBits;
	std::vector<uint8_t> elementsInOrder;

	getTreeRepresentation(root, treeInBits, elementsInOrder);

	writer.writeNumber(treeInBits.size(), BITS_FOR_TREE_SIZE);
	writer.writeBitChain(treeInBits);
	writer.writeVector(elementsInOrder);
	writer.writeNumber(elementsInFile, BITS_FOR_ELEMENTS_COUNT);
}

static void writeEncodedElements(BinaryReader &reader, BinaryWriter &writer, std::vector<BitChain> &encodedElements)
{
	while(!reader.eof())
	{
		writer.writeBitChain(encodedElements[reader.readByte()]);
	}
}

void encode(std::istream &inputStream, std::ostream &outputStream)
{
	BinaryReader reader(inputStream);
	BinaryWriter writer(outputStream);

	std::vector<uint64_t> elementsCount;
	uint64_t elementsInFile = countElements(reader, elementsCount);

	node* root = buildTreeFromWeights(elementsCount);

	std::vector<BitChain> encodedElements;
	{
		BitChain tmp;
		encodedElements.resize(ELEMENTS_NUMBER);
		encodeElements(root, tmp, encodedElements);
	}

	writeMetaInfo(root, elementsInFile, writer);

	reader.reset();
	writeEncodedElements(reader, writer, encodedElements);

	delete root;
}
