#include <iostream>

typedef unsigned long long uint64;

inline uint64 & count( int n, int k ) {
    static uint64 counts[50*101];
    
    if ( k > n ) k = n;
    
    return counts[ n * ( n - 1 ) / 2 + k - 1 ];
}

inline uint64 countSum( int n, int k ) {
    uint64 sum = 0;
    
    for ( int i = 1; i <= k; ++ i )
        sum += count( n-i, i );
    
    return sum;
}

int main() {
    count( 1, 1 ) = 1;
    for ( int n = 2; n <= 100; ++ n ) {
        count( n, 1 ) = 1;
        for ( int k = 1; k < n; ++ k )
            count( n, k ) = countSum( n, k );
        count( n, n ) = 1 + countSum( n, n-1 );
    }
    
    std::cout << count( 100, 100 ) - 1 << std::endl;

    return 0;
}