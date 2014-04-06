#pragma once

#include <ctime>

const int BITS_IN_BYTE = 8;
const unsigned char FULL_BYTE = 0xff;
const int BYTES_IN_INT = 4;
const int TYPE_SHIFT = 0;
const int NAME_SHIFT = 1;
const int NAME_LENGTH = 11;
const int TIME_SHIFT = NAME_SHIFT + NAME_LENGTH;
const int SIZE_SHIFT = TIME_SHIFT + sizeof(time_t);
const int DATA_LIST_SHIFT = SIZE_SHIFT + BYTES_IN_INT;
