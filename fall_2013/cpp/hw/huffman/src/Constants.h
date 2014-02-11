#pragma once

#include <cstdlib>
#include <cstdint>

const int BITS_IN_BYTE = 8;
const int BITS_FOR_TREE_SIZE = 2 * BITS_IN_BYTE;
const int BITS_FOR_ELEMENTS_COUNT = 8 * BITS_IN_BYTE;

const int ELEMENTS_NUMBER = (1<<BITS_IN_BYTE);

const int READER_BUFFER_SIZE = 200;
const int WRITER_BUFFER_SIZE = 200;
