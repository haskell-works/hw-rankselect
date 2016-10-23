
# Rank 64-bit
Count bits set (rank) from the most-significant bit upto a given position

The following finds the the rank of a bit, meaning it returns the sum of bits that are set to 1 from the most-signficant bit downto the bit at the given position.

    uint64_t v;       // Compute the rank (bits set) in v from the MSB to pos.
    unsigned int pos; // Bit position to count bits upto.
    uint64_t r;       // Resulting rank of bit at pos goes here.

    // Shift out bits after given position.
    r = v >> (sizeof(v) * CHAR_BIT - pos);
    // Count set bits in parallel.
    // r = (r & 0x5555...) + ((r >> 1) & 0x5555...);
    r = r - ((r >> 1) & ~0UL/3);
    // r = (r & 0x3333...) + ((r >> 2) & 0x3333...);
    r = (r & ~0UL/5) + ((r >> 2) & ~0UL/5);
    // r = (r & 0x0f0f...) + ((r >> 4) & 0x0f0f...);
    r = (r + (r >> 4)) & ~0UL/17;
    // r = r % 255;
    r = (r * (~0UL/255)) >> ((sizeof(v) - 1) * CHAR_BIT);

Juha Järvi sent this to me on November 21, 2009 as an inverse operation to the computing the bit position with the given rank, which follows.

Select the bit position (from the most-significant bit) with the given count (rank)

Source: https://graphics.stanford.edu/~seander/bithacks.html

# Select 64-bit
The following 64-bit code selects the position of the rth 1 bit when counting from the left. In other words if we start at the most significant bit and proceed to the right, counting the number of bits set to 1 until we reach the desired rank, r, then the position where we stop is returned. If the rank requested exceeds the count of bits set, then 64 is returned. The code may be modified for 32-bit or counting from the right.

    uint64_t v;          // Input value to find position with rank r.
    unsigned int r;      // Input: bit's desired rank [1-64].
    unsigned int s;      // Output: Resulting position of bit with rank r [1-64]
    uint64_t a, b, c, d; // Intermediate temporaries for bit count.
    unsigned int t;      // Bit count temporary.

    // Do a normal parallel bit count for a 64-bit integer,                     
    // but store all intermediate steps.                                        
    // a = (v & 0x5555...) + ((v >> 1) & 0x5555...);
    a =  v - ((v >> 1) & ~0UL/3);
    // b = (a & 0x3333...) + ((a >> 2) & 0x3333...);
    b = (a & ~0UL/5) + ((a >> 2) & ~0UL/5);
    // c = (b & 0x0f0f...) + ((b >> 4) & 0x0f0f...);
    c = (b + (b >> 4)) & ~0UL/0x11;
    // d = (c & 0x00ff...) + ((c >> 8) & 0x00ff...);
    d = (c + (c >> 8)) & ~0UL/0x101;
    t = (d >> 32) + (d >> 48);
    // Now do branchless select!                                                
    s  = 64;
    // if (r > t) {s -= 32; r -= t;}
    s -= ((t - r) & 256) >> 3; r -= (t & ((t - r) >> 8));
    t  = (d >> (s - 16)) & 0xff;
    // if (r > t) {s -= 16; r -= t;}
    s -= ((t - r) & 256) >> 4; r -= (t & ((t - r) >> 8));
    t  = (c >> (s - 8)) & 0xf;
    // if (r > t) {s -= 8; r -= t;}
    s -= ((t - r) & 256) >> 5; r -= (t & ((t - r) >> 8));
    t  = (b >> (s - 4)) & 0x7;
    // if (r > t) {s -= 4; r -= t;}
    s -= ((t - r) & 256) >> 6; r -= (t & ((t - r) >> 8));
    t  = (a >> (s - 2)) & 0x3;
    // if (r > t) {s -= 2; r -= t;}
    s -= ((t - r) & 256) >> 7; r -= (t & ((t - r) >> 8));
    t  = (v >> (s - 1)) & 0x1;
    // if (r > t) s--;
    s -= ((t - r) & 256) >> 8;
    s = 65 - s;

If branching is fast on your target CPU, consider uncommenting the if-statements and commenting the lines that follow them.
Juha Järvi sent this to me on November 21, 2009.

Source: https://graphics.stanford.edu/~seander/bithacks.html
