CXX = g++
CXXFLAGS = -O2 -Wall -std=c++0x

all : bin bin/arj

bin/arj : $(addprefix bin/, BinaryReader.o BinaryWriter.o BitChain.o Decoder.o Encoder.o HuffmanTree.o main.o)
	$(CXX) $^ -o $@

bin :
	mkdir -p bin

bin/BinaryReader.o : $(addprefix src/, BinaryReader.cpp BinaryReader.h BitChain.h Constants.h)
	$(CXX) $(CXXFLAGS) -c $< -o $@

bin/BinaryWriter.o : $(addprefix src/, BinaryWriter.cpp BinaryWriter.h BitChain.h Constants.h)
	$(CXX) $(CXXFLAGS) -c $< -o $@

bin/BitChain.o : $(addprefix src/, BitChain.cpp BitChain.h Constants.h)
	$(CXX) $(CXXFLAGS) -c $< -o $@

bin/Decoder.o : $(addprefix src/, Decoder.cpp HuffmanTree.h BitChain.h Constants.h BinaryReader.h BinaryWriter.h)
	$(CXX) $(CXXFLAGS) -c $< -o $@

bin/Encoder.o : $(addprefix src/, Encoder.cpp BinaryWriter.h BitChain.h Constants.h BinaryReader.h HuffmanTree.h)
	$(CXX) $(CXXFLAGS) -c $< -o $@

bin/HuffmanTree.o : $(addprefix src/, HuffmanTree.cpp HuffmanTree.h BitChain.h Constants.h)
	$(CXX) $(CXXFLAGS) -c $< -o $@

bin/main.o : $(addprefix src/, main.cpp Encoder.h Decoder.h)
	$(CXX) $(CXXFLAGS) -c $< -o $@


.PHONY: clean

clean : 
	rm -f $(addprefix bin/, BinaryReader.o BinaryWriter.o BitChain.o Decoder.o Encoder.o HuffmanTree.o main.o) bin/arj

