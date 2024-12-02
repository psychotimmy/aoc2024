# Advent of Code 2024

Fortran 77 on RaspberryPi OS (Debian bookworm)

Use (with gfortran installed) f77 filename.f -o filename to compile and link.

## Notes

Day 2 answers use the XOR intrinsic (generates a warning message from the compiler), which is not part of the f77 standard but is widely implemented. Input lines are read as strings and then parsed into an array of integers. This is required as the puzzle input integers are variable length but not right justified.

Day 3 - re-jigging the while loop in PROCESS() to eliminate the repeated code before the loop starts would improve the look of the code, but this is the unoptimised version that found the answers. (Cleaner versions are day3-1opt.f and day3-2opt.f respectively).
