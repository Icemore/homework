#include <iostream>
#include <string>

#include "HuffmanTree.h"
#include "BitChain.h"
#include "BinaryReader.h"
#include "BinaryWriter.h"

static void readMetaInfo(BinaryReader &reader, node*& root, uint64_t &elementsInFile)
{
	int treeSize = 0;
	BitChain treeInBits;
	std::vector<uint8_t> elementsInOrder;

	treeSize = (int)reader.readNumber(BITS_FOR_TREE_SIZE);
	reader.readBitChain(treeInBits, treeSize);
	reader.readVector(elementsInOrder, treeInBits.numberOfOnes());
	elementsInFile = reader.readNumber(BITS_FOR_ELEMENTS_COUNT);

	root = buildTreeFromRepresentation(treeInBits, elementsInOrder);
}

static void writeDecodedElements(BinaryReader &reader, BinaryWriter &writer, uint64_t elementsInFile, node* root)
{
	node* currentNode = root;
	uint64_t currentElementIndex = 0;

	while(currentElementIndex < elementsInFile)
	{
		bool curBit = reader.readBit();

		if(!curBit)
			currentNode = currentNode->left;
		else
			currentNode = currentNode->right;

		if(currentNode->isElementNode)
		{
			writer.writeByte(currentNode->element);
			currentNode = root;
			++currentElementIndex;
		}
	}
}

void decode(std::istream &inputStream, std::ostream &outputStream)
{
	BinaryReader reader(inputStream);
	BinaryWriter writer(outputStream);
	
	node* root = 0;
	uint64_t elementsInFile = 0;

	readMetaInfo(reader, root, elementsInFile);
	writeDecodedElements(reader, writer, elementsInFile, root);

	delete root;
}
