Both codes implement sha256 hashing algorithm.
bitcoin_hash1 uses a serial implementation.
bitcoin_hash2 uses a parallel implementation generating copies of hashBOI.

bitcoin_hash2 uses less clock cycles and is much faster but requires much more area.