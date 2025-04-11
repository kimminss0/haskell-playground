# Benchmark

Calculate the 1,000,000,000th Fibonacci number, modulo
18,446,744,073,709,551,557[^1].

[^1]: The largest unsigned 64 bit prime number (2^64 - 59).

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
    - **Version**: 18.1.6
    - **Options**: `-O2`
  - **GHC**:
    - **Version**: 9.4.8
    - **Options**: `-O2`
  - **GHC (LLVM Backend)**:
    - **Version**: 9.4.8
    - **Options**: `-O2 -fllvm -pgmlc=llc14 -pgmlo=opt14`
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
    - **Version**: 12.2.0
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
|C++ (clang++)|0.96|
|Haskell (GHC, LLVM Backend)|0.90|
|Haskell (GHC)|3.86|

> **Note:** Haskell (GHC w/ LLVM BE) outperforms C++ (clang++).

### FreeBSD, AMD64 (Ryzen 5 PRO 4650U, 32 GB RAM)

|Language (Compiler Used)|Execution Time (ms)|
|------------------------|-------------------|
|C++ (clang++)|0.84|
|Haskell (GHC, LLVM Backend)|0.76|
|Haskell (GHC)|7.04|

> **Note:** Haskell (GHC w/ LLVM BE) outperforms C++ (clang++).

### FreeBSD, AMD64 (Ryzen 7 PRO 2700U, 8 GB RAM)

> **TODO:** Update

|Language (Compiler Used)|Execution Time (ms)|
|------------------------|-------------------|
|C++ (clang++)|~2.39~|
|Haskell (GHC, LLVM Backend)|~2.40~|
|Haskell (GHC)|~7.44~|

### Linux, AMD64 (Ryzen 7 PRO 2700U, 8 GB RAM)

|Language (Compiler Used)|Execution Time (ms)|
|------------------------|-------------------|
|C++ (g++)|2.153|
|Haskell (GHC, LLVM Backend)|0.829|
|Haskell (GHC)|7.44|

> **Note:** Haskell (GHC w/ LLVM BE) outperforms C++ (g++).
