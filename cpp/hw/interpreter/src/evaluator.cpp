#include "evaluator.h"

#include <cstdlib>

const std::map<action::action_type, evaluator::executor> evaluator::execMethods = evaluator::initExecMethods();


std::map<action::action_type, evaluator::executor> evaluator::initExecMethods()
{
	std::map<action::action_type, evaluator::executor> execMethods;

	execMethods[action::CONST] = &evaluator::executeConst;
	execMethods[action::VAR_GET] = &evaluator::executeGetVar;
	execMethods[action::VAR_DECL] = &evaluator::executeVarDecl;
	execMethods[action::ADD_OP] = &evaluator::executeBinaryArithmOp;
	execMethods[action::UNARY_MINUS] = &evaluator::executeUnaryMinus;
	execMethods[action::MUL_OP] = &evaluator::executeBinaryArithmOp;
	execMethods[action::JMP] = &evaluator::executeJmp;
	execMethods[action::JZ] = &evaluator::executeJmpIfZero;
	execMethods[action::COND_OP] = &evaluator::executeBinaryCondition;
	execMethods[action::RET] = &evaluator::executeReturn;
	execMethods[action::CALL] = &evaluator::executeFunctionCall;
	execMethods[action::READ] = &evaluator::executeRead;
	execMethods[action::PRINT] = &evaluator::executePrint;
	execMethods[action::POP] = &evaluator::executePop;

	return execMethods;
}

bool evaluator::execute()
{
	try
	{
		executeList(unit.main);
	}
	catch(std::exception)
	{
		return false;
	}

	return true;
}

void evaluator::executePop(action &act, int &pos)
{
	st.pop();
}

void evaluator::executePrint(action &act, int &pos)
{
	std::cout << st.top() << std::endl;
	st.pop();
}

void evaluator::executeRead(action &act, int &pos)
{
	std::cin >> getVariable(act) ;
}

void evaluator::executeFunctionCall(action &act, int &pos)
{
	int funcId = -1;

	for(size_t i = 0; i < unit.functions.size(); ++i)
	{
		if(unit.functions[i].name == act.strArg)
		{
			funcId = i;
			break;
		}
	}

	if(funcId == -1)
	{
		raiseError(act, "undefined function " + act.strArg + ".");
	}

	function &func = unit.functions[funcId];

	if((int)func.args.size() != act.intArg)
	{
		raiseError(act, "arguments number mismatch for " + act.strArg + ".");
	}

	std::map<std::string, int> oldLocalVars = localVars;
	localVars.clear();
	for(int i = (int)func.args.size()-1; i>=0; --i)
	{
		localVars[func.args[i]] = st.top();
		st.pop();
	}

	++inFunction;
	executeList(func.program);
	--inFunction;

	localVars = oldLocalVars;
}

void evaluator::executeReturn(action &act, int &pos)
{
	pos = -2; // to get pos+1 < 0 and terminate execution loop
}

void evaluator::executeBinaryCondition(action &act, int &pos)
{
	int right = st.top(); 
	st.pop();
	int left = st.top();
	st.pop();
	bool res = false;

	switch (act.intArg)
	{
	case token::LESS:
		res = left < right;
		break;

	case token::LEQ:
		res = left <= right;
		break;

	case token::GREATER:
		res = left > right;
		break;

	case token::GREQ:
		res = left >= right;
		break;

	case token::EQUAL:
		res = left == right;
		break;

	case token::NEQ:
		res = left != right;
		break;

	default:
		break;
	}

	if(res)
	{
		st.push(1);
	}
	else
	{
		st.push(0);
	}
}

void evaluator::executeJmpIfZero(action &act, int &pos)
{
	int val = st.top();
	st.pop();

	if(val == 0)
	{
		pos = act.intArg - 1;
	}
}

void evaluator::executeJmp(action &act, int &pos)
{
	pos = act.intArg - 1;
}

void evaluator::executeUnaryMinus(action &act, int &pos)
{
	st.top() = -st.top();
}

void evaluator::executeBinaryArithmOp(action &act, int &pos)
{
	int right = st.top(); 
	st.pop();
	int left = st.top();
	st.pop();
	int res = 0;

	switch(act.intArg)
	{
	case token::ADD:
		res = left + right;
		break;

	case token::SUB:
		res = left - right;
		break;

	case token::MUL:
		res = left * right;
		break;

	case token::DIV:
		if(right == 0)
		{
			raiseError(act, "division by zero.");
		}

		res = left / right;
		break;

	default:
		break;
	}

	st.push(res);
}

void evaluator::executeVarDecl(action &act, int &pos)
{
	std::string name = act.strArg;
	int val = st.top();
	st.pop();

	if(inFunction)
	{
		localVars[name] = val;
	}
	else
	{
		globalVars[name] = val;
	}
}

void evaluator::executeConst(action &act, int &pos)
{
	int val = atoi(act.strArg.c_str());
	st.push(val);
}

int& evaluator::getVariable(action &act)
{
	std::string name = act.strArg;

	if(localVars.count(name) == 0 && globalVars.count(name) == 0)
	{
		raiseError(act, "undefined variable " + name +".");
	}

	if(localVars.count(name))
	{
		return localVars[name];
	}
	else
	{
		return globalVars[name];
	}
}

void evaluator::executeGetVar(action &act, int &pos)
{
	std::string name = act.strArg;
	int val = getVariable(act);

	st.push(val);
}

void evaluator::executeList(std::vector<action> &actionList)
{
	int pos = 0;
	while(pos>=0 && pos < (int)actionList.size())
	{
		executeAction(actionList[pos], pos);
		++pos;
	}
}

void evaluator::executeAction(action &act, int &pos)
{
	(this->*(execMethods.at(act.type)))(act, pos);
}

void evaluator::raiseError(action &reason, std::string msg)
{
	std::cerr << "line " << reason.line << ": " << msg << std::endl;
	throw std::exception();
}
