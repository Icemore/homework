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

	token(std::string val, int line)
		: val(val), line(line)
	{
		if(keywordsMap.count(val)!=0)
		{
			type = keywordsMap.at(val);
		}
		else
		{
			if(isdigit(val[0]))
			{
				type = CONST;

				// check int
				bool fail = false;
				for(size_t i = 0; i < val.size(); ++i)
				{
					if(!isdigit(val[i]))
						fail = true;
				}

				if(fail)
				{
					type = ERROR;
					val = "bad integer: " + val;
				}
			}
			else if(isalpha(val[0]))
			{
				type = ID;
			}
			else
			{
				// not a keyword, not an integer and not an identifier, so it's an error
				type = ERROR;
				val = "unexpected character: " + val;
			}
		}
	}


private:
	static std::map<std::string, tokenType> initMap()
	{
		std::map<std::string, tokenType> keywordsMap;

		keywordsMap["+"] = ADD;
		keywordsMap["-"] = SUB;
		keywordsMap["*"] = MUL;
		keywordsMap["/"] = DIV;
		keywordsMap["def"] = DEF;
		keywordsMap["while"] = WHILE;
		keywordsMap["if"] = IF;
		keywordsMap["read"] = READ;
		keywordsMap["print"] = PRINT;
		keywordsMap["end"] = END;	
		keywordsMap["return"] = RET;
		keywordsMap["\n"] = EOL;
		keywordsMap["<"] = LESS;
		keywordsMap[">"] = GREATER;
		keywordsMap["<="] = LEQ;
		keywordsMap[">="] = GREQ;
		keywordsMap["=="] = EQUAL;
		keywordsMap["!="] = NEQ;
		keywordsMap[":"] = COLON;
		keywordsMap["="] = ASSIGN;
		keywordsMap["("] = OPENPAR;
		keywordsMap[")"] = CLOSEPAR;
		keywordsMap[","] = COMMA;

		return keywordsMap;
	}

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

	std::string readAlphaNum()
	{
		std::string cur = curLine.substr(pos, 1);
		++pos;

		while(pos < curLine.size() && (isalnum(curLine[pos]) || curLine[pos]=='_'))
		{
			cur += curLine[pos];
			++pos;
		}

		return cur;
	}

	std::string readSymbol()
	{
		std::string cur = curLine.substr(pos, 1);
		++pos;

		// read one more char for <= >= != ==
		if((cur == "<" || cur == ">" || cur == "!" || cur == "=") && pos<curLine.size() && curLine[pos]=='=')
		{
			cur+='=';
			++pos;
		}

		return cur;
	}

	bool hasMoreTokens()
	{
		return !end;
	}

	token getNextToken()
	{
		if(end)
		{
			return token(token::EOF_, "", line);
		}

		skipSpaces();
		if(pos == curLine.size())
		{
			getLine();
			return token("\n", line);
		}

		if(curLine[pos] == '#')
		{
			// skip comments
			pos = curLine.size();
			return getNextToken();
		}

		// read token
		std::string cur;

		if(isalnum(curLine[pos]))
		{
			cur = readAlphaNum();
		}
		else
		{
			cur = readSymbol();
		}
		

		return token(cur, line);
	}

private:
	

	void getLine()
	{
		if(is_.eof())
		{
			end = true;
		}
		else
		{
			std::getline(is_, curLine);
			++line;
			pos = 0;
		}
	}

	void skipSpaces()
	{
		while(pos < curLine.size() && isspace(curLine[pos]))
			++pos;
	}

	std::istream &is_;
	size_t line, pos;
	bool end;
	std::string curLine;
};