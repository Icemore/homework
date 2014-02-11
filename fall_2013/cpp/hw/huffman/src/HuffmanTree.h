#pragma once

#include <queue>
#include "BitChain.h"

struct node
{
	bool isElementNode;
	long long weight;
	uint8_t element;
	
	node* left;
	node* right;

	~node()
	{
		if(left) delete left;
		if(right) delete right;
	}

	static node* getNewElementNode(uint8_t element=0, long long weight = 0)
	{
		return new node(true, weight, element);
	}

	static node* getNewSupportNode(node* left = 0, node* right = 0, long long weight = 0)
	{
		return new node(false, weight, 0, left, right);
	}

private:
	node(bool isElementNode, long long weight = 0, uint8_t element = 0, node* left = 0, node* right = 0)
		: isElementNode(isElementNode), weight(weight), element(element), left(left), right(right)
	{}
};

struct nodeCmp{
	bool operator()(node* first, node* second)
	{
			return first->weight > second->weight;
	}
};

typedef std::priority_queue<node*, std::vector<node*>, nodeCmp> nodeQueue;

void getTreeRepresentation(node* v, BitChain& bits, std::vector<uint8_t> &elementsInOrder);
node* buildTreeFromRepresentation(BitChain& bits, std::vector<uint8_t> &elementsInOrder);
node* buildTreeFromWeights(std::vector<uint64_t> &elementsCount);
