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

	static std::map<action::action_type, executor> initExecMethods()
	{
		std::map<action::action_type, executor> execMethods;

		execMethods[action::CONST] = &executeConst;
		execMethods[action::VAR_GET] = &executeGetVar;
		execMethods[action::VAR_DECL] = &executeVarDecl;
		execMethods[action::ADD_OP] = &executeBinaryArithmOp;
		execMethods[action::UNARY_MINUS] = &executeUnaryMinus;
		execMethods[action::MUL_OP] = &executeBinaryArithmOp;
		execMethods[action::JMP] = &executeJmp;
		execMethods[action::JZ] = &executeJmpIfZero;
		execMethods[action::COND_OP] = &executeBinaryCondition;
		execMethods[action::RET] = &executeReturn;
		execMethods[action::CALL] = &executeFunctionCall;
		execMethods[action::READ] = &executeRead;
		execMethods[action::PRINT] = &executePrint;
		execMethods[action::POP] = &executePop;

		return execMethods;
	}

	void execute()
	{
		executeList(unit.main);
	}

	void executePop(action &act, int &pos)
	{
		st.pop();
	}

	void executePrint(action &act, int &pos)
	{
		std::cout << st.top() << std::endl;
		st.pop();
	}

	void executeRead(action &act, int &pos)
	{
		std::cin >> getVariable(act) ;
	}

	void executeFunctionCall(action &act, int &pos)
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

		if(func.args.size() != act.intArg)
		{
			raiseError(act, "arguments number mismatch for " + act.strArg + ".");
		}

		std::map<string, int> oldLocalVars = localVars;
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

	void executeReturn(action &act, int &pos)
	{
		pos = -2; // to get pos+1 < 0 and terminate execution loop
	}

	void executeBinaryCondition(action &act, int &pos)
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

	void executeJmpIfZero(action &act, int &pos)
	{
		int val = st.top();
		st.pop();

		if(val == 0)
		{
			pos = act.intArg - 1;
		}
	}

	void executeJmp(action &act, int &pos)
	{
		pos = act.intArg - 1;
	}

	void executeUnaryMinus(action &act, int &pos)
	{
		st.top() = -st.top();
	}

	void executeBinaryArithmOp(action &act, int &pos)
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

	void executeVarDecl(action &act, int &pos)
	{
		string name = act.strArg;
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

	void executeConst(action &act, int &pos)
	{
		int val = atoi(act.strArg.c_str());
		st.push(val);
	}

	int& getVariable(action &act)
	{
		string name = act.strArg;
		int val = -1;

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

	void executeGetVar(action &act, int &pos)
	{
		string name = act.strArg;
		int val = getVariable(act);

		st.push(val);
	}

	void executeList(std::vector<action> &actionList)
	{
		int pos = 0;
		while(pos>=0 && pos < actionList.size())
		{
			executeAction(actionList[pos], pos);
			++pos;
		}
	}

	void executeAction(action &act, int &pos)
	{
		(this->*(execMethods.at(act.type)))(act, pos);
	}

	void raiseError(action &reason, std::string msg)
	{
		std::cerr << "line " << reason.line << ": " << msg << std::endl;
		throw std::exception();
	}

private:
	static const std::map<action::action_type, executor> execMethods;

	int inFunction;
	std::map<string, int> globalVars;
	std::map<string, int> localVars;
	programm &unit;
	std::stack<int> st;
};