#include <iostream>
#include <string>
#include <fstream>
#include <unistd.h>

#include "Encoder.h"
#include "Decoder.h"

void printUsageAndExit()
{
	std::cerr << "Usage: ./arj [-c | -d] -i <filename> -o <filename>" << std::endl;
	exit(EXIT_FAILURE);
}

int main(int argc, char **argv)
{
	int optionChar = 0;
	
	std::string inputFileName = "";
	std::string outputFileName = "";
	bool compression = false, decompression = false;

	while((optionChar = getopt(argc, argv, "cdi:o:")) != EOF)
		switch(optionChar)
		{
			case 'c' : 
				compression = true; 
				break;
			case 'd' : 
				decompression = true;
				break;
			case 'i':
				inputFileName = std::string(optarg);
				break;
			case 'o':
				outputFileName = std::string(optarg);
				break;
			case '?':
				printUsageAndExit();
				break;
		}

	if((compression && decompression) || 
	   (!compression && !decompression) ||
	   inputFileName == "" ||
	   outputFileName == "")
	{
		printUsageAndExit();
	}

	std::ifstream ifs(inputFileName, std::ios::binary);
	std::ofstream ofs(outputFileName, std::ios::binary);
	
	if(compression)
		encode(ifs, ofs);

	if(decompression)
		decode(ifs, ofs);

	ifs.close();
	ofs.close();

	exit(EXIT_SUCCESS);	
}
