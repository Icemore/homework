#include "streams.h"

int main()
{
    FileStream fout("out.txt");
    ConsoleWriter wout(fout);
    PPStream pout(wout);

    pout.write(5235);
    pout.write("Hello!");
}
