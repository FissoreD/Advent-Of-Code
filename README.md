1. Build the project : `dune build`
2. Exec the main : `dune exec aoc YY D P` 
   - YY the two last digits of the year
   - D the day number
   - P = 1 | 2 will run the second problem of the year (else the first part is run)
   - For example : 
     - `dune exec aoc 18 1 2` will run the second problem of the first day of 2018
     - `dune exec aoc 17 15 1` will run the first problem of the fifteenth day of 2017
3. Create new day : `dune exec make YY DD`