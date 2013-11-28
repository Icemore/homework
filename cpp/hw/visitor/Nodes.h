#pragma once

#include "Visitor.h"

class Node
{
public:
	virtual void visit(Visitor& visitor) = 0;
    virtual ~Node(){}
};

class BinaryNode : public Node
{
public:
	BinaryNode(Node* left, Node* right) 
		: left_(left), right_(right)
	{}

	virtual ~BinaryNode()
	{
		delete left_;
		delete right_;
	}

	Node* getLeft()
	{
		return left_;
	}

	Node* getRight()
	{
		return right_;
	}

private:
	Node* left_;
	Node* right_;
};

class Division : public BinaryNode
{
public:
	Division(Node* left, Node* right) : BinaryNode(left, right)
	{}

	virtual void visit(Visitor& visitor)
	{
		visitor.visitDivision(this);
	}
};

class Multiplication : public BinaryNode
{
public:
	Multiplication(Node* left, Node* right) : BinaryNode(left, right)
	{}

	virtual void visit(Visitor& visitor)
	{
		visitor.visitMultiplication(this);
	}
};

class Subtraction : public BinaryNode
{
public:
	Subtraction(Node* left, Node* right) : BinaryNode(left, right)
	{}

	virtual void visit(Visitor& visitor)
	{
		visitor.visitSubtraction(this);
	}
};

class Addition : public BinaryNode
{
public:
	Addition(Node* left, Node* right) : BinaryNode(left, right)
	{}

	virtual void visit(Visitor& visitor)
	{
		visitor.visitAddition(this);
	}
};

class Number : public Node
{
public:
	Number(int num) : number_(num)
	{}

	virtual void visit(Visitor& visitor)
	{
		visitor.visitNumber(this);
	}

	int getNumber()
	{
		return number_;
	}

private:
	int number_;
};
