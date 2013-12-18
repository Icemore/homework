#include <iostream>
#include <fstream>
#include <string>
#include <vector>
using namespace std;

#include "lexer.h"
#include "parser.h"
#include "evaluator.h"

string names[]={"CONST",
"VAR_GET",
"VAR_DECL",
"ADD_OP",
"UNARY_MINUS",
"MUL_OP",
"JMP",
"JZ",
"COND_OP",
"RET",
"CALL",
"READ",
"PRINT",
"POP"};

int main()
{
	ifstream ifs("in.txt");
	
	lexer lex(ifs);
	parser parse(lex);
	programm pr;

	try{
		parse.parse(pr);

	for(size_t i = 0; i < pr.main.size(); ++i)
	{
		cout << i <<": " << names[pr.main[i].type] << " " << pr.main[i].intArg << " " <<pr.main[i].strArg<< endl;
	}

	for(size_t j = 0; j < pr.functions.size(); ++j)
	{
		cout << endl;
		cout << "function " << pr.functions[j].name << endl;
		for(size_t i = 0; i < pr.functions[j].program.size(); ++i)
		{
			cout << i <<": " << names[pr.functions[j].program[i].type] << " " << pr.functions[j].program[i].intArg << " " <<pr.functions[j].program[i].strArg<< endl;
		}
	}

	cout << "======" <<endl;

	evaluator eval(pr);

	eval.execute();
	}
	catch(...)
	{}

	return 0;
}