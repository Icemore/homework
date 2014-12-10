#pragma once

#include <stdint.h>

inline uint16_t in_checksum(uint16_t *buffer, int size) {
    uint32_t cksum = 0;

    while(size > 1) {
        cksum += *buffer++;
        size -= sizeof(uint16_t);
    }

    if(size) {
        cksum += *(uint8_t*)buffer;
    }

    cksum = (cksum >> 16) + (cksum & 0xffff);
    cksum += (cksum >>16);

    return (uint16_t)(~cksum);
}
