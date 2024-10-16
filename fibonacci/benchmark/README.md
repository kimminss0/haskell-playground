# Benchmark

Calculate the 1,000,000,000th Fibonacci number, modulo 4,294,967,295[^1].

[^1]: The largest unsigned 32 bit prime number.
    [Reference](https://en.wikipedia.org/wiki/4,294,967,295).

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
|C++|2.30|
|Haskell (LLVM Backend)|2.31|
|Haskell|3.86|

### FreeBSD, AMD64

|Language|Execution Time (ms)|
|--------|------------------|
|C++|2.26|
|Haskell (LLVM Backend)|2.27|
|Haskell|2.27|
