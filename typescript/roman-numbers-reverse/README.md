# Roman Numbers Reverse way

Implemented by TDD with the help of the great people of @SCC_at_DATEV 
and friends at a Coding Dojo.

## What's hot?

* uses mocha/chai
* written in TypeScript
* TSLint compliant

## What's left to do?

* I am not happy with the feature envy in Convert.ts. 
  The digit conversion should be part of Result.ts, 
  which should be renamed to be a little less generic.
* The convert calls inside of Convert.ts represent a hidden loop
  violating OCP.
* Implement the other roman digits (L, C, D, M).
* Find a better way for digit conversion than that awkward while loop 
  (maybe using tail recursion or a filter-map-reduce-chain).

## How to run

```
npm install
npm test
```

## Some test cases

### addition rules

* 1 -> I
* 2 -> II
* 3 -> III
* 5 -> V
* 6 -> VI

### subtraction rules

* 4 -> IV
* 9 -> IX
