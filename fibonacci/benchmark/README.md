# Benchmark

Calculate the 10,000,000,000th Fibonacci number, modulo 18,446,744,073,709,551,557[^1].

[^1]: The largest unsigned 64 bit prime number. [Reference](https://www.mersenneforum.org/node/2899?p=62781#post62781).

## Environment

Tested on two environments:

### MacOS, AArch64

- **OS**: MacOS 15.0, Darwin Kernel Version 24.0.0
- **Processor**: Apple M2
- **Architecture**: AArch64
- **RAM**: 24 GB
- **Compiler**
  - **clang++**
    - **Language**: C++
    - **Version**: 16.0.0
    - **Options**: -O2
  - **GHC**: 
    - **Language**: Haskell
    - **Version**: 9.4.8
    - **Options**: -O2
  - **GHC (LLVM Backend)**: 
    - **Language**: Haskell
    - **Version**: 9.4.8
    - **Options**: -O2 -fllvm
    - **Note**: Uses LLVM backend (version 14.0.6)

### FreeBSD, AMD64

- **OS**: FreeBSD 14.1-RELEASE
- **Processor**: AMD Ryzen 5 PRO 4650U
- **Architecture**: AMD64
- **RAM**: 32 GB
- **Compiler**
  - **clang++**
    - **Language**: C++
    - **Version**: 16.0.6
    - **Options**: -O2
  - **GHC**: 
    - **Language**: Haskell
    - **Version**: 9.4.8
    - **Options**: -O2
  - **GHC (LLVM Backend)**: 
    - **Language**: Haskell
    - **Version**: 9.4.8
    - **Options**: -O2 -fllvm
    - **Note**: Uses LLVM backend (version 14.0.6)

## Result

The results are the mean of 5 executions.

### MacOS, AArch64

|Language|Execution Time (ms)|
|--------|------------------|
|C++|8.65|
|Haskell (LLVM Backend)|8.74|
|Haskell|38.18|

### FreeBSD, AMD64

|Language|Execution Time (ms)|
|--------|------------------|
|C++|7.53|
|Haskell (LLVM Backend)|7.54|
|Haskell|7.54|
