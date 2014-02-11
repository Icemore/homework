CXX=g++
CXXFLAGS=-Wall
DEPS = Nodes.h Visitor.h
OBJ = main.o Evaluator.o Printer.o

%.o: %.c $(DEPS)
		$(CXX) -c -o $@ $< $(CFLAGS)

main: $(OBJ)
		$(CXX) -o $@ $^ $(CFLAGS)

.PHONY: clean
clean:
	rm -f main $(OBJ)
