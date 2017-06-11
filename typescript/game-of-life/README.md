# Game of Life

full implementation of Game of Life using TypeScript.

## What's hot?

* functional implementation of all algorithms
* fluent interface naming
* value objects for all artifacts
* usage of a hash-based set for storing the living cells
* infinite board

## What I learned

* how to implement a more complex kata in TypeScript
* functional way to implement a cartesian product (see Cartesian.ts).
* efficient refactoring with WebStorm
* some TSLint rules are not that great, but can be easily modified:
    * member ordering: why should private static methods come before anything else?
    * no unused expression: collides with the assertions from Chai
    
## How to run the tests

```bash
$ npm install
$ npm test
```