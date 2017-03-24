# simplify-bkg-tests
Tests for FIT FLP project #1

## Running the tests
You must be located in the tests/ folder. *Please mind that the script uses flags without a dash!*
```Bash
bash testrunner.sh [1|2|i] (optional)
```
The option specifies the argument for simplify-bkg program. Defaults to `2`.

`testrunner.sh` runs all the tests found in testfiles folder and are marked as input files (see below the naming convetions)

The script assumes several things:
* the binary is in parent folder (`../`)
* the binary is named simplify-bkg
* tests are in testfiles/ folder
* the naming is as follows
    * `{testnumber}-test.in` for input file
    * `{testnumber}-test-1.out` for output of the first algorithm
    * `{testnumber}-test-2.out` for output of the second algorithm

## Resources (mainly for tests)
http://www.sanfoundry.com/automata-theory-cfg-eliminating-useless-symbols/
https://en.wikipedia.org/wiki/Useless\_rules
http://kilby.stanford.edu/~rvg/154/handouts/useless.html
https://www.tutorialspoint.com/automata\_theory/cfg\_simplification.htm
