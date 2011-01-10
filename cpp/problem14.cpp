#include <iostream>

typedef unsigned long long uint64;

const uint64 count = 1000000;

int main() {
    uint64 maxLen = 0;
    uint64 maxNum = 0;

    for ( uint64 start = 1; start < count; ++ start ) {
        uint64 cur = start;
        uint64 len = 1;

        while ( cur != 1 ) {
            cur = (cur & 1) ? (cur << 1) + cur + 1 : (cur >> 1);
            len ++;
        }

        if ( len > maxLen ) {
            maxLen = len;
            maxNum = start;
        }
    }

    std::cout << maxNum << "/" << maxLen << std::endl;

    return 0;
}