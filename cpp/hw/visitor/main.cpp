#include <iostream>
#include "Visitor.h"
#include "Nodes.h"

int main()
{
    Division root(
        new Multiplication(
            new Subtraction(
                new Number(1),
                new Number(3)),
            new Number(2)),
        new Addition(
            new Number(-5),
            new Addition(
                new Subtraction(
                    new Number(1),
                    new Number(5)),
                new Number(10))));

    Printer printer;
    root.visit(printer);
    std::cout << std::endl;

    Evaluator evaluator;
    root.visit(evaluator);
    std::cout << evaluator.extractResult() << std::endl;

    return 0;
}
