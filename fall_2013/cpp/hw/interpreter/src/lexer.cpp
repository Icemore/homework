#include "lexer.h"

const std::map<std::string, token::tokenType> token::keywordsMap = token::initMap();

std::map<std::string, token::tokenType> token::initMap()
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

token::token(std::string val, int line)
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

void lexer::getLine()
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

void lexer::skipSpaces()
{
	while(pos < curLine.size() && isspace(curLine[pos]))
		++pos;
}

token lexer::getNextToken()
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

std::string lexer::readAlphaNum()
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

std::string lexer::readSymbol()
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