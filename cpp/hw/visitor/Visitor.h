#pragma once

class Visitor
{
public:
	virtual void visitDivision(class Division* node) = 0;
	virtual void visitMultiplication(class Multiplication* node) = 0;
	virtual void visitSubtraction(class Subtraction* node) = 0;
	virtual void visitAddition(class Addition* node) = 0;
	virtual void visitNumber(class Number* node) = 0;
};

class Printer : public Visitor
{
public:
	virtual void visitAddition(class Addition* node);
	virtual void visitDivision(class Division* node);
	virtual void visitMultiplication(class Multiplication* node);
	virtual void visitSubtraction(class Subtraction* node);
	virtual void visitNumber(class Number* node);

private:
	virtual void printBinaryNode(class BinaryNode*, char operation);
};

class Evaluator : public Visitor
{
public:
	int extractResult();

	virtual void visitAddition(class Addition* node);
	virtual void visitDivision(class Division* node);
	virtual void visitMultiplication(class Multiplication* node);
	virtual void visitSubtraction(class Subtraction* node);
	virtual void visitNumber(class Number* node);

private:
	virtual void evaluateBinaryNodeChilds(class BinaryNode*, int &leftRes, int &rightRes);

	int result_;
};