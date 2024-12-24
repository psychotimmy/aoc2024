# Advent of Code 2024

Fortran 77 (mostly - some extensions and f90 style recursion used) on RaspberryPiOS (Debian bookworm)

Use (with gfortran installed) f77 filename.f -o filename to compile and link.

## Notes

Day 1 - insertion sort is fast enough for this dataset, quicker to code than other sorting algorithms and doesn't require f77 standard-breaking recursion like a vanilla quicksort algorithm.

Day 2 answers use the XOR intrinsic (generates a warning message from the compiler), which is not part of the f77 standard but is widely implemented. Input lines are read as strings and then parsed into an array of integers. This is required as the puzzle input integers are variable length but not right justified.

Day 3 - re-jigging the while loop in PROCESS() to eliminate the repeated code before the loop starts would improve the look of the code, but this is the unoptimised version that found the answers. (Cleaner versions are day3-1opt.f and day3-2opt.f respectively).

Day 4 - there's a simple optimisation for part 1 as we only need to check around the 'X' cells in the grid and we also don't need to check that it's an 'X' again when we are in the PARSE() function. Revised code is day4-1opt.f and takes approximately half the execution time of my original day4-1.f. In day4-2.f the PARSE() function can be improved by removing a couple of surplus additions - see day4-2opt.f

Day 5 - fairly ugly answers even for f77, but effective! I especially enjoyed implementing a bubblesort algorithm for part 2.

Day 6 - In part 2 there's a big assumption made about how many times a square can be visited before we can 'safely' declare that a loop has been found. A more accurate way would be to check that a particular path has been completely followed at least twice in a row, but that would take a lot more effort than making the assumption that if you see a square 'n' times there's a loop. Another more accurate method would be to record the direction each square is encountered. If you pass through the same square twice in the same direction then there's a loop. Hence why my assumption of n=10 works, and even n=5 should always work. It does for my input, but I've left n=10 in the code as that's what was there when I found the correct answer. There's some fairly ugly use of COMMON and the grid walking code should really be contained within a function rather than appearing twice. 

Day 7 - slowest part 2 of the year so far on a Pi 5 - takes around 1.5 seconds to execute, vs 0.05 seconds for part 1. Re-wrote my STR2INTARRAY subroutine from day 2 to use an internal READ to make it more elegant. opt versions shave about 0.01s off part 1 and 0.1s off part 2 by checking that the sum hasn't exceeded the target value before all of the operators are applied. Solutions avoid recursion so stay within the f77 standard! It's a nice feature of f77 that if you use the division operator on two integers it always results in the integer part of the division - e.g. 1/4, 1/2, 7/8 all evaluate to 0. Part 2 can be sped up by another 0.03s or so by using the eval routine from part 1 before calling the eval routine in part 2 - i.e. only doing the longer check on input that failed the part 1 check. It's not really a worthwhile saving given the additional code required, so it's not included in the day7-2opt.f version.

Day 8 - Complex numbers are very useful as grid co-ordinates today. Both parts run in 0.001s or less!

Day 9 - I spent far too long using the same data structure for part 1 in part 2, before realising I was never going to understand the tweaked algorithm as it was too complex. Changing the data structure for part 2 worked wonders. Part 1 = 0.003 seconds, Part 2 = 0.1s, elapsed time between getting part 1 and part 2 right = 12 hours!!

Day 10 - I've broken two of my conventions today. First of all day10.f answers both parts (like many others, I accidentally did part 2 before doing part 1, but it was simpler to bodge the code to do a proper part 1 without disturbing what eventually was the answer for part 2). Second - this isn't standard f77 as I've used a recursive function, permitted by Fortran90. If I get time towards the end of the month I'll redo this as an iterative solution - it shouldn't be too difficult.

Day 11 - The only difference between part 1 and 2 is the iteration count (25 vs 75). There's clearly a very bad way to do part 1, but I avoided that bullet! 0.003s for part 1 and around 0.5s for part 2. At the start of the problem I thought I may have needed to sort the list of stones, but the solution is fast enough without that complication. I guess that sorting may even slow the solution down depending on how/when any list is sorted.

Day 12 - Not standard f77 as I've used a recursive subroutine courtesy of f90. Part 1 is relatively straightforward, part 2 had me looking at all sorts of complications (e.g. keeping a list of points and working out when the direction changed - yuk) until I realised that it was simply asking you to count the number of corners. I think my solution is reasonably elegant. The algorithm counts exterior corners (i.e. a clockwise 270 degree turn) and interior corners (a clockwise 90 degree turn) to ensure that all corners are counted exactly once. Execution time for each part is around 0.005s.

Day 13 - Part 1 was done before breakfast (takes 0.04s to execute) and the extension in part 2 wasn't a surprise ... and clearly the method I used in part 1 would be too inefficient for part 2! Had breakfast, realised that there was only one answer for each input in part 1, so solving the necessary simultaneous equations was straightforward to get the answer for part 2. Unfortunately, dealing with the oddities of numerical precision in f77 wasn't! Final answer works without needing the -fno-range-check flag in gfortran (f77), the equations need double precision variables to calculate N and M (real*8 won't hack it all of the time it as I found to my cost) and the check at the end makes use of the INT8 conversion rather than INT for (now) obvious reasons. I got there in the end. Part 2 code solves the puzzle in 0.003 seconds (and solves part 1 with OFFSET=0 in the same time). Fun - eventually!

Day 14 - Scruffy but effective code. For part 2 I managed to reduce the number of grids written to the file for visual Christmas tree searching by making a couple of wrong guesses. I'd originally assumed that the tree would be symmetrical around the central Y axis. All that talk about quadrants in part 1 misled me beautifully. Needless to say the tree isn't! In the end, exhaustive searching was my friend for this puzzle. Part 2 takes around a second to write the file to disk. I used vi to search for a bunch of contiguous X's. I enjoyed this puzzle possibly more than I should have done. Happy Christmas!

Day 15 - part 1 completed fairly easily, but a different approach is required for part 2. Still trying to figure that one out! There are currently 2 variants here that work on the (large) sample data but fail with my puzzle data. The code is horrendous, and I could do with re-writing it from the ground up. Something for later.

Day 16 - to do

Day 17 - part 1 straightforward, part 2 took a lot of working out with pen and paper and currently still has my program input hardcoded, so not included in this repository for the moment.

Day 18 - First time implementing an A* algorithm in f77. Made lots of silly errors writing it (hence the slight surplus of loop variable names) but got there in the end. Part 2 is a simple extension and could easily be improved by either (a) searching backwards or (b) using a binary chop to more quickly land on the first case that fails. However, stepping through from 1024 blocks onwards is interesting to see the number of steps required gradually increase and then ... blam! Too slow - you can't escape now! Note - although I've used the A* algorithm it's actually Dijkstra as the heuristic is set to 0 - plenty fast enough for today's problem. But it's nice to have the more broadly useful algorithm in the bag.

Day 19 - Part 1 done, but code not here yet as I should be able to create a combined version for both parts once I've figured out how solve part 2. The sample data shows up a wrinkle in my code. For example, rrb can be made in 2 ways (r,r,b or r,rb) but what I've written only counts it as one way. It's the memoization that's going wrong, as if I take it out and just use recursion on small strings it gets the right answer. Recursive solution is the most sensible method, so f90.

Day 20 - Part 1 solution day20-1m.f is the first that completed with the correct answer, although it wasn't the first one I wrote. The difference between the two was implementing a Manhattan distance heuristic within the A* function and also realising that there were (at most) only 2 possible worthwhile cheats (No point just advancing or retreating 2 steps on the path as that can't save 100 picoseconds). It takes 64 minutes to run! I can already see how to improve this and I will need to if part 2 is to complete in a reasonable time! The improved version for part 1 is day20-1.f and takes 5m 36s on my hardware. day20-1opt.f is the same - but improves the A* algorithm by reversing the search order of the open and closed nodes list, bringing the execution time down to 3m 20s. However ... we're no need to keep recalculating new routes using A*, as we've stored the single path and each steps distance to the end as the index of ROUTE!!!! day20-1final.f runs in just over a second (even with some of the earlier optimisations taken out to make the code clearer). So this is the version that day20-2.f is based on. It runs in 1m 25s, which can clearly still be optimised by putting some of the earlier optimisations back in from the less than optimal part 1 solutions, but, meh.

Day 21 - to do

Day 22 - to do

Day 23 - Part 1 isn't the most compact solution, nor the most elegant, but worked first time after I'd realised it was looking for computers that *started* with a 't', rather than *containing* a 't', which results in too high a count! Part 2 followed my hunch that as the input data had a specific shape (each computer connected to 13 others), then the maximum size of the LAN party would also be 13. Easiest way to find this out from part 1 was to work out which computers appeared most often in a triplet. This gives 13 sets of 14 computers. The password is the intersection of these sets ... the 13 computers we're looking for will appear in all 13 sets. Not an ounce of recursion needed, so a pure f77 implementation. Part 2 completes in 2.4 seconds, plenty fast enough. I reckon that general networks that don't share these characteristics would be harder to work with ... an NP complete problem space perhaps? Pleased to have reused my insertion sort from day 1 in part 2 - small victories!

Day 24 - to do
