#include <iostream>

typedef unsigned long long uint64;

const int maxLen = 100;

inline uint64 & countIncr( int len, int start ) {
    static uint64 counts[maxLen*10];
        
    return counts[ (len - 1) * 10 + start ];
}

inline uint64 countIncrSum( int len, int start ) {
    uint64 sum = 0;
    
    for ( int digit = start; digit <= 9; ++ digit )
        sum += countIncr( len - 1, digit );
    
    return sum;
}

inline uint64 countNotBouncy( int len ) {
    return countIncr( len, 0 ) + countIncr( len, 1 ) - 10;
}

inline uint64 countNotBouncyBelow( int len ) {
    uint64 sum = 0;
    
    for ( int i = 1; i <= len; ++ i )
        sum += countNotBouncy( i );
    
    return sum;
}

int main() {
    for ( int start = 0; start <= 9; ++ start )
        countIncr( 1, start ) = 10 - start;
        
    for ( int len = 2; len <= 100; ++ len ) {
        for ( int start = 0; start <= 9; ++ start )
            countIncr( len, start ) = countIncrSum( len, start );
    }
    
    std::cout << countNotBouncyBelow( 6 ) << std::endl;
    std::cout << countNotBouncyBelow( 10 ) << std::endl;    
    std::cout << countNotBouncyBelow( 100 ) << std::endl;

    return 0;
}