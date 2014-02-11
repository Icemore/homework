#pragma once

#include <iostream>
#include <string>
#include <sstream>
#include <map>

struct token
{
	enum tokenType
	{
		ADD,
		SUB,
		MUL,
		DIV,
		DEF,
		WHILE,
		IF,
		EOL,
		READ,
		PRINT,
		END,
		ID,
		CONST,
		LESS,
		LEQ,
		GREATER,
		GREQ,
		EQUAL,
		NEQ,
		COLON,
		ASSIGN,
		OPENPAR,
		CLOSEPAR,
		COMMA,
		RET,
		NOP,
		EOF_,
		ERROR
	};

	tokenType type;
	std::string val;
	int line;

	token() {}

	token(tokenType type, std::string val, int line)
		: type(type), val(val), line(line)
	{}

	token(std::string val, int line);

private:
	static std::map<std::string, tokenType> initMap();
	static const std::map<std::string, tokenType> keywordsMap;
};

class lexer
{
public:
	lexer(std::istream &is)
		: is_(is), line(0), pos(0), end(false)
	{
		getLine();
	}

	bool hasMoreTokens() { return !end;	}
	token getNextToken();

private:
	void getLine();
	void skipSpaces();
	std::string readAlphaNum();
	std::string readSymbol();

	std::istream &is_;
	size_t line, pos;
	bool end;
	std::string curLine;
};
