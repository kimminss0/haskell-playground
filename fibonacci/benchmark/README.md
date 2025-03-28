# Benchmark

Calculate the 1,000,000,000th Fibonacci number, modulo 4,294,967,295[^1].

[^1]: The largest unsigned 32 bit prime number.
    [Reference](https://en.wikipedia.org/wiki/4,294,967,295).

## Environment

Tested on four environments.

### MacOS, AArch64 (Apple M2, 24 GB RAM)

- **OS**: MacOS 15.0 (Darwin Kernel 24.0.0)
- **Processor**: Apple M2
- **Architecture**: AArch64
- **RAM**: 24 GB
- **Compiler**
  - **clang++**
    - **Version**: 16.0.0
    - **Options**: `-O2`
  - **GHC**:
    - **Version**: 9.4.8
    - **Options**: `-O2`
  - **GHC (LLVM Backend)**:
    - **Version**: 9.4.8
    - **Options**: `-O2 -fllvm`
    - **Note**: Uses LLVM backend (version 14.0.6)

### FreeBSD, AMD64 (Ryzen 5 PRO 4650U, 32 GB RAM)

- **OS**: FreeBSD 14.1-RELEASE
- **Processor**: AMD Ryzen 5 PRO 4650U
- **Architecture**: AMD64
- **RAM**: 32 GB
- **Compiler**
  - **clang++**
    - **Version**: 18.1.5
    - **Options**: `-O2`
  - **GHC**:
    - **Version**: 9.4.8
    - **Options**: `-O2`
  - **GHC (LLVM Backend)**:
    - **Version**: 9.4.8
    - **Options**: `-O2 -fllvm`
    - **Note**: Uses LLVM backend (version 14.0.6)

### FreeBSD, AMD64 (Ryzen 7 PRO 2700U, 8 GB RAM)

- **OS**: FreeBSD 14.1-RELEASE
- **Processor**: AMD Ryzen 7 PRO 2700U
- **Architecture**: AMD64
- **RAM**: 8 GB
- **Compiler**
  - **clang++**
    - **Version**: 18.1.5
    - **Options**: `-O2`
  - **GHC**:
    - **Version**: 9.4.8
    - **Options**: `-O2`
  - **GHC (LLVM Backend)**:
    - **Version**: 9.4.8
    - **Options**: `-O2 -fllvm`
    - **Note**: Uses LLVM backend (version 14.0.6)

### Linux, AMD64 (Ryzen 7 PRO 2700U, 8 GB RAM)

- **OS**: Fedora 40 (Linux Kernel 6.8.5)
- **Processor**: AMD Ryzen 7 PRO 2700U
- **Architecture**: AMD64
- **RAM**: 8 GB
- **Compiler**
  - **g++**
    - **Version**: 14.0.1
    - **Options**: `-O2`
  - **GHC**:
    - **Version**: 9.4.8
    - **Options**: `-O2`
  - **GHC (LLVM Backend)**:
    - **Version**: 9.4.8
    - **Options**: `-O2 -fllvm`
    - **Note**: Uses LLVM backend (version 14.0.5)

## Result

The results are the mean of 5 executions.

### MacOS, AArch64 (Apple M2, 24 GB RAM)

|Language (Compiler Used)|Execution Time (ms)|
|------------------------|-------------------|
|C++ (clang++)|2.30|
|Haskell (GHC, LLVM Backend)|2.31|
|Haskell (GHC)|3.86|

### FreeBSD, AMD64 (Ryzen 5 PRO 4650U, 32 GB RAM)

|Language (Compiler Used)|Execution Time (ms)|
|------------------------|-------------------|
|C++ (clang++)|2.26|
|Haskell (GHC, LLVM Backend)|2.27|
|Haskell (GHC)|2.27|

### FreeBSD, AMD64 (Ryzen 7 PRO 2700U, 8 GB RAM)

|Language (Compiler Used)|Execution Time (ms)|
|------------------------|-------------------|
|C++ (clang++)|2.39|
|Haskell (GHC, LLVM Backend)|2.40|
|Haskell (GHC)|7.44|

### Linux, AMD64 (Ryzen 7 PRO 2700U, 8 GB RAM)

|Language (Compiler Used)|Execution Time (ms)|
|------------------------|-------------------|
|C++ (g++)|2.40|
|Haskell (GHC, LLVM Backend)|2.40|
|Haskell (GHC)|7.44|
