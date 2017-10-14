# Rock Paper Scissors

Hopelessly over-engineered version of RPS in Java 8.

## What's hot

* No ifs in production code at all
* Declarative way of defining our winning rules
* Declarative way of defining the rules for win, draw, loss
* Fluent interface assertion for determining if a choice wins against 
another one
* Using higher-order functions instead of stupid classes to 
determine which rule actually applies.

## What sucks (ranting about Java)

Unfortunately, some constructs in Java are rather cumbersome
I think I could write the same code in a more modern language
with half as much code.

* collecting objects to a map
* creating and querying repeatable annotations 
* defining and invoking higher-order functions is even worse than in C# 
(which is kinda hard ;-)