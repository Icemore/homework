CXX = g++
CXXFLAGS = -O2 -Wall -std=c++0x
LDFLAGS = -lboost_system -lboost_filesystem -lboost_serialization -lboost_program_options

SRCDIR = src
BINDIR = obj

EXE = $(patsubst $(SRCDIR)/%_main.cpp,%,$(wildcard $(SRCDIR)/*_main.cpp))
SOURCES = $(filter-out %_main.cpp,$(wildcard $(SRCDIR)/*.cpp))
OBJECTS = $(patsubst $(SRCDIR)/%.cpp,$(BINDIR)/%.o,$(SOURCES))

all: $(BINDIR) $(EXE)

$(EXE) : % : $(BINDIR)/%_main.o $(OBJECTS)
	$(CXX) $^ -o $@ $(LDFLAGS)
	
VPATH = $(SRCDIR)
$(BINDIR)/%.o: %.cpp
	$(CXX) $(CXXFLAGS) -c -MMD -o $@ $<

include $(wildcard $(BINDIR)/*.d)

$(BINDIR):
	mkdir -p $(BINDIR)

clean:
	rm -rf $(BINDIR) $(EXE)

.PHONY: clean all
