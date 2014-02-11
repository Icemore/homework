#pragma once

#include <vector>
#include <stack>
#include <map>

#include "parser.h"
#include "lexer.h"

class evaluator
{
public:
	evaluator(programm &unit)
		: unit(unit), inFunction(0)
	{}

	typedef void (evaluator::*executor)(action&, int&);

	bool execute();

private:
	static std::map<action::action_type, executor> initExecMethods();

	void executePop(action &act, int &pos);
	void executePrint(action &act, int &pos);
	void executeRead(action &act, int &pos);
	void executeFunctionCall(action &act, int &pos);
	void executeReturn(action &act, int &pos);
	void executeBinaryCondition(action &act, int &pos);
	void executeJmpIfZero(action &act, int &pos);
	void executeJmp(action &act, int &pos);
	void executeUnaryMinus(action &act, int &pos);
	void executeBinaryArithmOp(action &act, int &pos);
	void executeVarDecl(action &act, int &pos);
	void executeConst(action &act, int &pos);
	void executeGetVar(action &act, int &pos);
	void executeList(std::vector<action> &actionList);
	void executeAction(action &act, int &pos);

	int& getVariable(action &act);
	void raiseError(action &reason, std::string msg);


	static const std::map<action::action_type, executor> execMethods;

	std::map<std::string, int> globalVars;
	std::map<std::string, int> localVars;
	programm &unit;
  int inFunction;
	std::stack<int> st;
};
