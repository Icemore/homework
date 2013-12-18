#pragma once

#include <string>
#include <vector>

using std::string;
using std::vector;

#include "lexer.h"

struct action
{
	enum action_type
	{
		CONST,
		VAR_GET,
		VAR_DECL,
		ADD,
		SUB,
		UNARY_OP,
		MUL,
		DIV,
		JMP,
		JZ,
		COND,
		RET,
		POP
	};

	action(action_type type, int intArg, int line)
		: type(type), intArg(intArg), line(line)
	{}

	action(action_type type, std::string strArg, int line)
		: type(type), strArg(strArg), line(line)
	{}

	action_type type;
	int intArg;
	int line;
	std::string strArg;
};

struct function
{
	std::string name;
	std::vector<std::string> args;
	std::vector<action> program;
};

struct programm
{
	std::vector<function> functions;
	std::vector<action> main;
};

class parser
{
public:
	parser(lexer &lex)
		: lex(lex), parsingFailed(false)
	{}

	bool parse(programm &unit)
	{
		while(lex.hasMoreTokens())
		{
			token first = lex.getNextToken();

			if(first.type == token::DEF)
			{
				function newFunction = parse_function_declaration();
				unit.functions.push_back(newFunction);
			}
			else
			{
				parse_statement(first, unit.main);
			}
		}

		return !parsingFailed;
	}



private:
	function parse_function_declaration()
	{
		function newFunction;
		newFunction.name = expectToken(token::ID).val;

		expectToken(token::OPENPAR);

		parse_formal_arg_list(newFunction.args);

		expectToken(token::COLON);
		expectToken(token::EOL);

		parse_statement_list(newFunction.program);

		expectToken(token::EOL);

		return newFunction;
	}

	void parse_statement_list(std::vector<action> &actions)
	{
		token first = lex.getNextToken();

		while(first.type != token::END)
		{
			parse_statement(first, actions);
			first = lex.getNextToken();
		}
	}

	void parse_formal_arg_list(std::vector<std::string> &args)
	{
		token first = lex.getNextToken();

		if(first.type == token::CLOSEPAR)
		{
			return;
		}

		if(first.type == token::ID)
		{
			args.push_back(first.val);
			parse_formal_arg_list_tail(args);
		}
		else
		{
			raiseError(first);
		}
	}

	void parse_formal_arg_list_tail(std::vector<std::string> &args)
	{
		token first = lex.getNextToken();

		if(first.type == token::CLOSEPAR)
		{
			return;
		}

		if(first.type == token::COMMA)
		{
			token name = expectToken(token::ID);
			args.push_back(name.val);
			parse_formal_arg_list_tail(args);
		}
		else
		{
			raiseError(first);
		}
	}

	void parse_statement(token first, std::vector<action> &actions)
	{
		switch (first.type)
		{
		case token::WHILE:
			parse_while(actions);
			break;

		case token::IF:
			parse_if(actions);
			break;

		case token::RET:
			parse_return(actions);
			break;

		default:
			break;
		}
			
	}

	void parse_while(std::vector<action> &actions)
	{
		int cond_index = actions.size();

		parse_condition(actions);

		// implement "if not condition jump to the end"
		int jmp_to_end_index = actions.size();
		actions.push_back(action(action::JZ, 0, -1));

		expectToken(token::COLON);
		expectToken(token::EOL);

		parse_statement_list(actions);

		expectToken(token::EOL);

		actions.push_back(action(action::JMP, cond_index, -1));
		actions[jmp_to_end_index].intArg = actions.size();
	}

	void parse_if(std::vector<action> &actions)
	{
		parse_condition(actions);

		// implement "if not condition jump to the end"
		int jmp_to_end_index = actions.size();
		actions.push_back(action(action::JZ, 0, -1));

		expectToken(token::COLON);
		expectToken(token::EOL);

		parse_statement_list(actions);

		expectToken(token::EOL);

		actions[jmp_to_end_index].intArg = actions.size();
	}

	void parse_condition(std::vector<action> &actions)
	{

	}

	void parse_return(std::vector<action> &actions)
	{
		parse_expression(actions);
		expectToken(token::EOL);
	}

	token parse_expression(std::vector<action> &actions)
	{
		
	}

	void parse_mult_expr(std::vector<action> &actions)
	{

	}


	token expectToken(token::tokenType type)
	{
		token cur = lex.getNextToken();
		
		if(cur.type != type)
		{
			raiseError(cur);
			parsingFailed = true;
		}

		return cur;
	}

	void raiseError(token reason)
	{
		if(reason.type == token::ERROR)
		{
			std::cerr << "line " << reason.line << ": lexical error." << std::endl;
			std::cerr << reason.val << std::endl;
		}
		else
		{
			std::cerr << "line " << reason.line << ": syntax error." << std::endl;
		}
	}

	bool parsingFailed;
	lexer &lex;
};