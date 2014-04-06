CXX = g++
CXXFLAGS = -Wall -std=c++0x -O2

EXE = $(BINDIR)/service.o
SRCDIR = .
BINDIR = bin

OBJECTS = $(patsubst $(SRCDIR)/%.cpp,$(BINDIR)/%.o,$(wildcard $(SRCDIR)/*.cpp))

all: $(BINDIR) $(EXE)

$(EXE): $(OBJECTS)
	ld -r -o $@ $^
	
VPATH = $(SRCDIR)
$(BINDIR)/%.o: %.cpp
	$(CXX) $(CXXFLAGS) -c -MMD -o $@ $<

include $(wildcard $(BINDIR)/*.d)

$(BINDIR):
	mkdir -p $(BINDIR)

clean:
	rm -rf $(BINDIR)

.PHONY: clean all
