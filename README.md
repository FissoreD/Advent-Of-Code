_Ocaml_

Every puzzle is coded in [`Ocaml`](https://v2.ocaml.org/manual/) and built with [`dune`](https://dune.readthedocs.io/en/latest/).

_Main_

1. Build the project : `dune build`
2. Exec the main : `dune exec main YY D P` 
   - YY the two last digits of the year
   - D the day number
   - P = 1 | 2 will run the second problem of the year (else the first part is run)
   - For example : 
     - `dune exec aoc 18 1 2` will run the second problem of the first day of 2018
     - `dune exec aoc 17 15 1` will run the first problem of the fifteenth day of 2017
3. Create new day : `dune exec make YY DD`

_Tests_
1. Build the project : `dune build`
2. Run all tests with `dune exec test`
3. Optional paramenters :
   1. `yXX` where XX is an year number, executes only the test of the passed year
   2. `dXX` where XX is a day number, executes only the test of day XX (it can be also one digit) of every year
   3. `yXX dYY` launch the puzzle of year XX and day YY.
4. The test also compute each puzzle execution time, an output exemple : 
    ``` 
    https://adventofcode.com/2017/day/7 
    Executed Year 17 - Day  7 - Part 1 -> Time : 0.07800
    Executed Year 17 - Day  7 - Part 2 -> Time : 0.06200
    https://adventofcode.com/2017/day/8
    Executed Year 17 - Day  8 - Part 1 -> Time : 0.00000
    Executed Year 17 - Day  8 - Part 2 -> Time : 0.00000
    https://adventofcode.com/2017/day/9        
    Executed Year 17 - Day  9 - Part 1 -> Time : 0.00000
    Executed Year 17 - Day  9 - Part 2 -> Time : 0.01600 
    ```
