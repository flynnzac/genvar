# rata: an R package for data manipulation in a Stata-like, "command-on-data" way

## Installation

To install `rata` type, at the R console,
```R
install.packages("devtools", dependencies=TRUE)
library(devtools)
install_github("flynnzac/rata")
library(rata)
```

## Motivation
The goal of this package is to remove one barrier to using R, a free software statistical package, for researchers in the social sciences who are used to Stata's model of data.  Stata assumes a rectangular model for data (there are observations and variables) while R allow us for more flexible data structures. But in the social sciences, data is almost always in the (observation,variable) framework, the Stata way of working with data is ingrained, and the additional flexibility of R can make things that are routine in Stata more difficult because the user has to know a much wider variety of functions to get the desired result.  This package solves this problem by implementing a Stata-like method for manipulating data in R so that the advantages of data manipulation are available in a free software package. 

The package has one active dataset and commands can be used to modify or reference variables from that dataset in more of a "command" format than R's standard function and object format. 

To get a feel for what `rata` looks like see the example in examples/test.r. The syntax is more intuitive than standard R to people who are used to think at the (observation, variable) level of a dataset.

## Current State

I started writing the package on September 20, 2018 so it is very much a work-in-progress, but it should mostly work. Report any bugs or feature requests  (if there are things from Stata that you would like to be ported to R) to the Github repo https://github.com/flynnzac/rata.

`rata` is a "command-on-data" environment for R and to develop a more complete regression package for common models that incorporates robust and clustered standard errors, time series operators, and fixed effects all into one estimation command (this is mostly tying together other R packages which use a function-object interface into a command-on-data interface).  The goal is not just to replicate Stata's environment, but to offer an improved command-on-data environment that takes advantage of the additional flexibility of R.

See below for the basic concepts and the reference manual for a list of commands.

# Basic concepts

## Variable lists


## Quoted Expressions
