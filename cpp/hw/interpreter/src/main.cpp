#include <iostream>
#include <fstream>
#include <string>
#include <vector>

#include "lexer.h"
#include "parser.h"
#include "evaluator.h"

int main(int argc, char **argv)
{
	std::ifstream in(argv[1]);

	programm unit;

	lexer lex(in);
	parser scanner(lex);

	if(!scanner.parse(unit))
		return 1;

	evaluator eval(unit);

	if(!eval.execute())
		return 1;

	return 0;
}