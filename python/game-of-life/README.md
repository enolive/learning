# Game of Life

This kata was initially done during the GDCR at SWEC17.
I finished the implementation afterwards using
Python, NoseTest and Behave for the Feature Specifications.

## What's hot?

* the feature files were the driver for any further implementation.
* I used smaller unit tests to implement small aspects of the functionality
  just like make a dump of the current board, counting the neighbours and so on.
* the board is implemented without any regard to the potential 
  width and height and is only limited to the system's memory size
  as we only memorize the living cells.