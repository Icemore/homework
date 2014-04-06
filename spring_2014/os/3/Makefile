CXX = g++
CXXFLAGS = -Wall -std=c++0x -O2

EXE = $(patsubst $(SRCDIR)/%.cpp,%,$(wildcard $(SRCDIR)/*.cpp))
SRCDIR = commands
BINDIR = bin
SERVDIR = service
SERVLIB = $(SERVDIR)/bin/service.o

OBJECTS = $(patsubst $(SRCDIR)/%.cpp,$(BINDIR)/%.o,$(wildcard $(SRCDIR)/*.cpp))

all: $(BINDIR) service $(EXE)

service :
	$(MAKE) -C $(SERVDIR)

$(EXE) : % : $(BINDIR)/%.o $(SERVLIB)
	$(CXX) $^ -o $@

VPATH = $(SRCDIR)
$(BINDIR)/%.o: %.cpp
	$(CXX) $(CXXFLAGS) -I $(SERVDIR) -c -MMD -o $@ $<

include $(wildcard $(BINDIR)/*.d)

$(BINDIR):
	mkdir -p $(BINDIR)

clean:
	$(MAKE) -C $(SERVDIR) clean
	rm -rf $(BINDIR) $(EXE)

.PHONY: clean all service
