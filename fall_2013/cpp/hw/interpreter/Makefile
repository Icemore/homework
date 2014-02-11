CXX = g++
CXXFLAGS = -O2 -Wall
EXE = pp

all : bin bin/$(EXE)

bin/$(EXE) : $(addprefix bin/, main.o evaluator.o lexer.o parser.o)
	$(CXX) $^ -o $@

bin :
	mkdir -p bin


bin/main.o: $(addprefix src/, main.cpp lexer.h parser.h evaluator.h)
	$(CXX) $(CXXFLAGS) -c $< -o $@

bin/evaluator.o : $(addprefix src/, evaluator.cpp evaluator.h parser.h lexer.h)
	$(CXX) $(CXXFLAGS) -c $< -o $@

bin/lexer.o : $(addprefix src/, lexer.cpp lexer.h)
	$(CXX) $(CXXFLAGS) -c $< -o $@

bin/parser.o : $(addprefix src/, parser.cpp parser.h lexer.h)
	$(CXX) $(CXXFLAGS) -c $< -o $@


.PHONY: clean

clean : 
	rm -f $(addprefix bin/, main.o evaluator.o lexer.o parser.o) bin/$(EXE)

