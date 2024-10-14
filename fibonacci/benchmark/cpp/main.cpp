#include <exception>
#include <iostream>
#include <string>

uint64_t fib(long long);
uint64_t fib1(long long, uint64_t, uint64_t);

const uint64_t mod = 18446744073709551557UL;

int main(int argc, char *argv[]) {
  if (argc < 2)
    return 1;
  std::string arg = argv[1];
  try {
    long long num = std::stol(arg);
    std::cout << fib(num) << std::endl;
  } catch (const std::exception &e) {
    return 1;
  }
  return 0;
}

uint64_t fib(long long num) { return fib1(num, 0, 1); }

uint64_t fib1(long long c, uint64_t a, uint64_t b) {
  if (c == 1)
    return b;
  // C++ supports tail call optimization.
  return fib1(c - 1, b % mod, (a + b) % mod);
}


