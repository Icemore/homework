CXX = g++
CXXFLAGS = -O2 -Wall -std=c++0x

EXE = a.out
SRCDIR = src
BINDIR = bin

OBJECTS = $(patsubst $(SRCDIR)/%.cpp,$(BINDIR)/%.o,$(wildcard $(SRCDIR)/*.cpp))

all: $(BINDIR) $(EXE)

$(EXE): $(OBJECTS)
	$(CXX) $^ -o $@
	
VPATH = $(SRCDIR)
$(BINDIR)/%.o: %.cpp
	$(CXX) $(CXXFLAGS) -c -MMD -o $@ $<

include $(wildcard $(BINDIR)/*.d)

$(BINDIR):
	mkdir -p $(BINDIR)

clean:
	rm -rf $(BINDIR) $(EXE)

.PHONY: clean all
