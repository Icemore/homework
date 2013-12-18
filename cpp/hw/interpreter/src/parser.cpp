#include "parser.h"

bool parser::parse(programm &unit)
{
	try{
		while(lex.hasMoreTokens())
		{
			token first = lex.getNextToken();

			if(first.type == token::EOF_)
				break;

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
	} 
	catch (std::exception)
	{
		return false;
	}

	return true;
}


function parser::parse_function_declaration()
{
	function newFunction;
	newFunction.name = expectToken(token::ID).val;

	expectToken(token::OPENPAR);

	parse_formal_arg_list(newFunction.args);

	expectToken(token::COLON);
	expectToken(token::EOL);

	parse_statement_list(newFunction.program);

	token eol = expectToken(token::EOL);
	newFunction.program.push_back(action(action::CONST, 0, eol.line));

	return newFunction;
}

void parser::parse_statement_list(std::vector<action> &actions)
{
	token first = lex.getNextToken();

	while(first.type != token::END)
	{
		parse_statement(first, actions);
		first = lex.getNextToken();
	}
}

void parser::parse_formal_arg_list(std::vector<std::string> &args)
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

void parser::parse_formal_arg_list_tail(std::vector<std::string> &args)
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

void parser::parse_statement(token first, std::vector<action> &actions)
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

	case token::READ:
		parse_read(actions);
		break;

	case token::PRINT:
		parse_print(actions);
		break;
		
	case token::EOL:
		break;

	case token::ID:
		parse_statement_with_ID(first, actions);
		break;

	default:
		parse_expr(first, actions);
		actions.push_back(action(action::POP, 0, first.line));
		break;
	}
			
}

void parser::parse_statement_with_ID(token first, std::vector<action> &actions)
{
	token next = lex.getNextToken();
		
	if(next.type == token::ASSIGN)
	{
		parse_var_decl(first, actions);
		return;
	}
		
	token last;
	switch (next.type)
	{
	case token::OPENPAR:
		parse_func_call(first, actions);
		last = lex.getNextToken();
		break;

	case token::ADD:
	case token::SUB:
		actions.push_back(action(action::VAR_GET, first.val, first.line));
		last = parse_expr(lex.getNextToken(), actions);
		actions.push_back(action(action::ADD_OP, next.type, next.line));
		break;

	case token::MUL:
	case token::DIV:
		actions.push_back(action(action::VAR_GET, first.val, first.line));
		last = parse_mult_expr(lex.getNextToken(), actions);
		actions.push_back(action(action::MUL_OP, next.type, next.line));
		break;

	default:
		raiseError(next);
		break;
	}

	actions.push_back(action(action::POP, 0, first.line));

	if(last.type != token::EOL)
	{
		raiseError(last);
	}
}

void parser::parse_var_decl(token name, std::vector<action> &actions)
{
	token last = parse_expr(lex.getNextToken(), actions);

	actions.push_back(action(action::VAR_DECL, name.val, name.line));

	if(last.type != token::EOL)
	{
		raiseError(last);
	}
}

void parser::parse_while(std::vector<action> &actions)
{
	int cond_index = actions.size();

	parse_condition(actions);

	// implement "if not condition jump to the end"
	int jmp_to_end_index = actions.size();
	actions.push_back(action(action::JZ, 0, -1));

	expectToken(token::EOL);

	parse_statement_list(actions);

	expectToken(token::EOL);

	actions.push_back(action(action::JMP, cond_index, -1));
	actions[jmp_to_end_index].intArg = actions.size();
}

void parser::parse_if(std::vector<action> &actions)
{
	parse_condition(actions);

	// implement "if not condition jump to the end"
	int jmp_to_end_index = actions.size();
	actions.push_back(action(action::JZ, 0, -1));

	expectToken(token::EOL);

	parse_statement_list(actions);

	expectToken(token::EOL);

	actions[jmp_to_end_index].intArg = actions.size();
}

bool parser::isCondition(token::tokenType type)
{
	return (type == token::LESS ||
			type == token::LEQ ||
			type == token::GREATER ||
			type == token::GREQ ||
			type == token::EQUAL ||
			type == token::NEQ );
}

void parser::parse_condition(std::vector<action> &actions)
{
	token cmp_op = parse_expr(lex.getNextToken(), actions);

	if(!isCondition(cmp_op.type))
	{
		raiseError(cmp_op);
	}

	token colon = parse_expr(lex.getNextToken(), actions);

	if(colon.type != token::COLON)
	{
		raiseError(colon);
	}

	actions.push_back(action(action::COND_OP, cmp_op.type, cmp_op.line));
}

void parser::parse_read(std::vector<action> &actions)
{
	token name = expectToken(token::ID);
	actions.push_back(action(action::READ, name.val, name.line));
	expectToken(token::EOL);
}

void parser::parse_print(std::vector<action> &actions)
{
	token next = parse_expr(lex.getNextToken(), actions);

	actions.push_back(action(action::PRINT, 0, next.line));

	if(next.type != token::EOL)
	{
		raiseError(next);
	}
}

void parser::parse_return(std::vector<action> &actions)
{
	token next = parse_expr(lex.getNextToken(), actions);
	if(next.type != token::EOL)
	{
		raiseError(next);
	}

	actions.push_back(action(action::RET, 0, next.line));
}

token parser::parse_expr(token first, std::vector<action> &actions)
{
	token next = parse_mult_expr(first, actions);

	while(next.type == token::ADD || next.type == token::SUB)
	{
		action curAction(action::ADD_OP, next.type, next.line);
		next = parse_mult_expr(lex.getNextToken(), actions);
		actions.push_back(curAction);
	}

	return next;
}

token parser::parse_mult_expr(token first, std::vector<action> &actions)
{
	token next = parse_unary_expr(first, actions);

	while(next.type == token::MUL || next.type == token::DIV)
	{
		action curAction(action::MUL_OP, next.type, next.line);
		next = parse_unary_expr(lex.getNextToken(), actions);
		actions.push_back(curAction);
	}

	return next;
}

token parser::parse_unary_expr(token first, std::vector<action> &actions)
{
	if(first.type == token::SUB)
	{
		token next = parse_base_expr(lex.getNextToken(), actions);
		actions.push_back(action(action::UNARY_MINUS, first.val, first.line));
		return next;
	}
	else
	{
		return parse_base_expr(first, actions);
	}
}

token parser::parse_base_expr(token first, std::vector<action> &actions)
{
	if(first.type == token::OPENPAR)
	{
		token next = parse_expr(lex.getNextToken(), actions);
		if(next.type != token::CLOSEPAR)
		{
			raiseError(next);
		}

		return lex.getNextToken();
	}
	else
	{
		return parse_operand(first, actions);
	}
}

token parser::parse_operand(token first, std::vector<action> &actions)
{
	if(first.type == token::CONST)
	{
		actions.push_back(action(action::CONST, first.val, first.line));
		return lex.getNextToken();
	}

	if(first.type == token::ID)
	{
		token next = lex.getNextToken();

		if(next.type == token::OPENPAR)
		{
			parse_func_call(first, actions);
			return lex.getNextToken();
		}
		else
		{
			actions.push_back(action(action::VAR_GET, first.val, first.line));
			return next;
		}
	}
	else
	{
		raiseError(first);
		return token(token::ERROR, 0, first.line);
	}
}

void parser::parse_func_call(token name, std::vector<action> &actions)
{
	int argsCnt = parse_fact_arg_list(actions);
	actions.push_back(action(action::CALL, name.val, argsCnt, name.line));
}

int parser::parse_fact_arg_list(std::vector<action> &actions)
{
	token first = lex.getNextToken();

	if(first.type == token::CLOSEPAR)
	{
		return 0;
	}

	token next = parse_expr(first, actions);

	return 1 + parse_fact_arg_list_tail(next, actions);
}

int parser::parse_fact_arg_list_tail(token first, std::vector<action> &actions)
{
	if(first.type == token::CLOSEPAR)
		return 0;

	if(first.type == token::COMMA)
	{
		token next = lex.getNextToken();
		next = parse_expr(next, actions);
		return 1 + parse_fact_arg_list_tail(next, actions);
	}
	else
	{
		raiseError(first);
		return 0;
	}
}

token parser::expectToken(token::tokenType type)
{
	token cur = lex.getNextToken();
		
	if(cur.type != type)
	{
		raiseError(cur);
	}

	return cur;
}

void parser::raiseError(token reason)
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

	throw std::exception();
}