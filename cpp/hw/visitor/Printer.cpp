#include <iostream>
#include "Visitor.h"
#include "Nodes.h"

void Printer::printBinaryNode(class BinaryNode* node, char operation)
{
	std::cout << '(';
	node->getLeft()->visit(*this);
	std::cout << ' ' << operation << ' ';
	node->getRight()->visit(*this);
	std::cout << ')';
}

void Printer::visitAddition(class Addition* node)
{
	this->printBinaryNode(node, '+');
}

void Printer::visitDivision(class Division* node)
{
	this->printBinaryNode(node, '/');
}

void Printer::visitMultiplication(class Multiplication* node)
{
	this->printBinaryNode(node, '*');
}

void Printer::visitSubtraction(class Subtraction* node)
{
	this->printBinaryNode(node, '-');
}

void Printer::visitNumber(class Number* node)
{
	std::cout << node->getNumber();
}