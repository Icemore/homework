#include <stack>

#include "HuffmanTree.h"
#include "BitChain.h"

static node* buildTreeFromQueue(nodeQueue &q)
{
	while(q.size() > 1)
	{
		node* left = q.top(); q.pop();
		node* right = q.top(); q.pop();

		node* newNode = node::getNewSupportNode(left, right, left->weight + right->weight);

		q.push(newNode);
	}

	node* root = 0;
	
	if(!q.empty())
		root = q.top();

	if(root == 0 || root->isElementNode)
		root = node::getNewSupportNode(0, root);

	return root;
}

node* buildTreeFromWeights(std::vector<uint64_t> &elementsCount)
{
	nodeQueue q;

	for(int element = 0; element < ELEMENTS_NUMBER; ++element)
	{
		if(elementsCount[element] > 0)
		{
			node* curNode = node::getNewElementNode(element, elementsCount[element]);

			q.push(curNode);
		}
	}
	
	return buildTreeFromQueue(q);
}

void getTreeRepresentation(node* v, BitChain& bits, std::vector<uint8_t> &elementsInOrder)
{
	if(v->left)
		getTreeRepresentation(v->left, bits, elementsInOrder);

	if(v->right)
		getTreeRepresentation(v->right, bits, elementsInOrder);

	if(!v->isElementNode)
    {
		bits.pushBit(0);
    }
	else
	{
		bits.pushBit(1);
		elementsInOrder.push_back(v->element);
	}
}

node* buildTreeFromRepresentation(BitChain& bits, std::vector<uint8_t> &elementsInOrder)
{
	std::stack<node*> st;
	
	int elementIndex = 0;
	for(size_t i = 0; i < bits.size(); ++i)
	{
		node* cur = bits[i] ? node::getNewElementNode() : node::getNewSupportNode();
		
		if(cur->isElementNode)
        {
			cur->element = elementsInOrder[elementIndex++];
        }
		else
		{
			if(!st.empty())
            {
				cur->right = st.top();
                st.pop();
            }

			if(!st.empty())
            {
				cur->left = st.top();
                st.pop();
            }
		}

		st.push(cur);
	}

	return st.top();
}
