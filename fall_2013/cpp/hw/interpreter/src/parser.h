#pragma once

#include <string>
#include <vector>

#include "lexer.h"

struct action
{
	enum action_type
	{
		CONST,
		VAR_GET,
		VAR_DECL,
		ADD_OP,
		UNARY_MINUS,
		MUL_OP,
		JMP,
		JZ,
		COND_OP,
		RET,
		CALL,
		READ,
		PRINT,
		POP
	};

	action(action_type type, std::string strArg, int intArg, int line)
		:  type(type), intArg(intArg),  strArg(strArg) ,line(line)
	{}

	action(action_type type, int intArg, int line)
		: type(type), intArg(intArg), line(line)
	{}

	action(action_type type, std::string strArg, int line)
		: type(type), strArg(strArg), line(line)
	{}

	action_type type;
	int intArg;
	std::string strArg;
	int line;
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
		: lex(lex)
	{}

	bool parse(programm &unit);

private:
	void parse_statement_list(std::vector<action> &actions);
	void parse_statement(token first, std::vector<action> &actions);
	void parse_statement_with_ID(token first, std::vector<action> &actions);

	function parse_function_declaration();
	void parse_formal_arg_list(std::vector<std::string> &args);
	void parse_formal_arg_list_tail(std::vector<std::string> &args);


	void parse_var_decl(token name, std::vector<action> &actions);
	void parse_while(std::vector<action> &actions);
	void parse_if(std::vector<action> &actions);
	void parse_read(std::vector<action> &actions);
	void parse_print(std::vector<action> &actions);
	void parse_return(std::vector<action> &actions);

	void parse_condition(std::vector<action> &actions);
	token parse_expr(token first, std::vector<action> &actions);
	token parse_mult_expr(token first, std::vector<action> &actions);
	token parse_unary_expr(token first, std::vector<action> &actions);
	token parse_base_expr(token first, std::vector<action> &actions);
	token parse_operand(token first, std::vector<action> &actions);

	void parse_func_call(token name, std::vector<action> &actions);
	int parse_fact_arg_list(std::vector<action> &actions);
	int parse_fact_arg_list_tail(token first, std::vector<action> &actions);

	token expectToken(token::tokenType type);

	void raiseError(token reason);
	bool isCondition(token::tokenType type);

	lexer &lex;
};
