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
The goal of this package is to remove one barrier to using R, a free software statistical package, for researchers in the social sciences who are used to Stata's model of data.  Stata assumes a rectangular model for data (there are observations and variables) while R allow us for more flexible data structures. There are advantages to R's additional flexibility, but in the social sciences, data is almost always in the (observation, variable) framework, the Stata way of working with data is ingrained, and the additional flexibility of R can make things that are routine in Stata more difficult because the user has to know a much wider variety of functions to get the desired result.  This package solves the problem by implementing a Stata-like method for manipulating data in R so that this data model (which I will call the "command-on-data" model) is available in a free software package. 

The package implements an environment where there is one active dataset and commands can be used to modify or reference variables from that dataset by issuing "commands" as opposed to R's standard environment (applying functions to modify objects).

To get a feel for what `rata` looks like see the example in examples/test.r. The syntax is more intuitive (well, hopefully) than standard R to people who are used to thinking at the (observation, variable) level of a dataset.

## Current State

I started writing the package on September 20, 2018 so it is very much a work-in-progress, but it should mostly work. Report any bugs or feature requests  (if there are features from Stata that you would like to be ported to this environment in R) to the Github repo https://github.com/flynnzac/rata.

`rata` is a "command-on-data" environment for R and it has a more complete regression package for common models which incorporates robust and clustered standard errors, time series operators, and fixed effects all into one estimation command (this is mostly tying together other R packages which use a function-object interface into a command-on-data interface).  The goal is not just to replicate Stata's environment, but to offer an improved command-on-data environment that takes advantage of the additional flexibility of R.

See below for the basic concepts and the reference manual for a list of commands.

# Unique `rata` variable types

## Variable lists

Variable lists in `rata` are specified by quoting the names of variables like, `"educ wage black"`. The names can be specified using wildcard characters as well. For example, if the variables "x100 x2 x3" make up the dataset, they can be all be included by specifying, `"x*"`. If we only want to list "x2 x3", then we can specify `"x?"` because `?` matches only one character.

## Quoted Expressions

Many `rata` commands work by using "quoted expressions" which are bits of code enclosed in quotation marks. For example, to use `rata`'s `gen` command to generate log wages, you might type `gen("lnwage", "log(wage)")`. The second argument is a quoted expression. The quotes are necessary so that `R` does not try to execute `log(wage)` outside of the `rata` environment. If you need to use a quotation mark in a quoted expression, escape it like so: `gen("hello", "\"hello\"")` to generate a variable called `hello` that contains the string _hello_ for every observation.

# Basic overview of currently available functions

Use the `use` function to load a dataset into the `rata` environment. 

Then, modify the dataset or add additional transformations of variables with the `gen` command.

Analyze the data using `summarize`, `reg` (for linear regression), `logit` (for binary regression), or execute arbitrary `R` code in the `rata` environment (so you can use the variable names directly in the R code) with the `do` command.  You can create a dataset of summary statistics with `collapse`.

There is support for panel data using `xtset` (`reg` works for panel regression as well, see its manual page) and `lag` can be used to generate lags and leads of variables in the panel.

The data can be reshaped from long-to-wide or from wide-to-long using the `shape` command.

`forvar` can apply code by variable to a certain variable list in the dataset.


# Licensing

`rata` is Free Software. It is licensed under version 3 of the GPL. You are free to use, modify, and redistribute the code, but not without restriction. If you produce a modified version of `rata`, it too must be made available under the terms of the GPL (its source code must be made available and others must also be free to use, modify, and redistribute it). The text of the license is included as the file LICENSE in this repository and at https://www.gnu.org/licenses/gpl-3.0.txt.
