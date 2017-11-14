# Bowling-Kata

Bowling kata done in python

## What's hot?

* learned how to use python 3
* usage of tuples, filter, map, reduce
* usage of higher-order functions for selecting the scoring rule

## What's not?

* scoring depends on the state of rolls. This would
  break the code if someone called it multi-threaded.
  It would be better to clone the rolls and move it to
  the existing tuple of ball_index and score.
* there might be some feature envy between the current score
  (the tuple) and the methods
  that work on it such as get_scoring_function or is_strike.
  Defining an explicit CurrentScore class might be better.
