#include "Visitor.h"
#include "Nodes.h"

void Evaluator::evaluateBinaryNodeChilds(class BinaryNode* node, int &leftRes, int &rightRes)
{
	node->getLeft()->visit(*this);
	leftRes = result_;

	node->getRight()->visit(*this);
	rightRes = result_;
}

void Evaluator::visitAddition(class Addition* node)
{
	int left, right;
	this->evaluateBinaryNodeChilds(node, left, right);
	result_ = left + right;
}

void Evaluator::visitDivision(class Division* node)
{
	int left, right;
	this->evaluateBinaryNodeChilds(node, left, right);
	result_ = left / right;
}

void Evaluator::visitMultiplication(class Multiplication* node)
{
	int left, right;
	this->evaluateBinaryNodeChilds(node, left, right);
	result_ = left * right;
}

void Evaluator::visitSubtraction(class Subtraction* node)
{
	int left, right;
	this->evaluateBinaryNodeChilds(node, left, right);
	result_ = left - right;
}

void Evaluator::visitNumber(class Number* node)
{
	result_ = node->getNumber();
}

int Evaluator::extractResult()
{
	return result_;
}