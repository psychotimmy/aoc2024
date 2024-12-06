# Advent of Code 2024

Fortran 77 on RaspberryPi OS (Debian bookworm)

Use (with gfortran installed) f77 filename.f -o filename to compile and link.

## Notes

Day 1 - insertion sort is fast enough for this dataset, quicker to code than other sorting algorithms and doesn't require f77 standard-breaking recursion like a vanilla quicksort algorithm.

Day 2 answers use the XOR intrinsic (generates a warning message from the compiler), which is not part of the f77 standard but is widely implemented. Input lines are read as strings and then parsed into an array of integers. This is required as the puzzle input integers are variable length but not right justified.

Day 3 - re-jigging the while loop in PROCESS() to eliminate the repeated code before the loop starts would improve the look of the code, but this is the unoptimised version that found the answers. (Cleaner versions are day3-1opt.f and day3-2opt.f respectively).

Day 4 - there's a simple optimisation for part 1 as we only need to check around the 'X' cells in the grid and we also don't need to check that it's an 'X' again when we are in the PARSE() function. Revised code is day4-1opt.f and takes approximately half the execution time of my original day4-1.f. In day4-2.f the PARSE() function can be improved by removing a couple of surplus additions - see day4-2opt.f

Day 5 - fairly ugly answers even for f77, but effective! I especially enjoyed implementing a bubblesort algorithm for part 2.

Day 6 - In part 2 there's a big assumption made about how many times a square can be visited before we can 'safely' declare that a loop has been found. A more accurate way would be to check that a particular path has been completely followed at least twice in a row, but that would take a lot more effort than making the assumption that if you see a square 'n' times there's a loop. My real input works for n=5, the code in this repository has n=10. There's some faitly ugly use of COMMON and the grid walking code should really be contained within a function rather than appearing twice. 
