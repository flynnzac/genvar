# rata: an R package for data manipulation in a Stata-like way

## Motivation
The goal of this package is to remove one barrier to using R, a free software statistical package, for users in the social sciences who are used to Stata's model of data.  Stata assumes a rectangular model for data (there are observations and variables) while R allow us for more flexible data structures. But in the social sciences, data is almost always in the (observation,variable) framework, the Stata way of working with data is ingrained, and the additional flexibility of R can make things that are routine in Stata more difficult because the user has to know a much wider variety of functions to get the desired result.  This package solves this problem by implementing a Stata-like method for manipulating data in R. 

The package has one active dataset and commands can be used to modify or reference variables from that dataset in more of a "command" format than R's standard function and object format. For example, in standard R to load a csv file and drop a column and keep only observations where some condition is satisfied,

```R
data <- read.csv("data.csv")
data[,"col1"] <- NULL
data <- data[highscool==1,]
```

Using `rtata`, the same can be written as,
```R
use("data.csv")
drop(~col1)
keep("highschool==1")
```

## Current State

I started writing the package on September 20, 2018.  The package is under active development, but is not yet ready for error-free use and it is not fully-documented.





