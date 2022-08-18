1. Build the project : `dune build`
2. Exec the main : `dune exec Advent_Of_Code YY DD P` 
   - YY the two last digits of the year
   - DD the two digits of the day (the 1st of the month will be 01)
   - P = 1 | 2 will run the second problem of the year (else the first part is run)
   - For example : 
     - `dune exec Advent_Of_Code 18 01 2` will run the second problem of the first day of 2018
     - `dune exec Advent_Of_Code 17 05 1` will run the first problem of the fifth day of 2017
3. Create new day : `dune exec make YY DD`