# rata: an R package for data manipulation in a Stata-like way

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

To get a feel for what `rata` looks like see the following example,
```R
rm(list=ls(all=TRUE))
library(rata)
data(Produc)

## "use" loads the data set Produc into memory
use(Produc)
listif()

## preserve data set
p <- preserve()

## list variables in dataset
describe()

## sum over emp by year
collapse(~sum(emp)|year)

## restore original data
restore(p)

## reshape dataset from (state,year,emp) to (state,emp1970,emp1971,...)
shape(state~emp|year, direction="wide")

## listif(expr) prints the dataset if the statement is true (it also returns the part of the dataset
## that satisfies the condition)
listif()

## reshape dataset from (state, emp1970, emp1971,...) to (state,year,emp)
shape(state~year|emp, direction="long")
listif()

## list emp by year just for Wyoming
listif("state == 'WYOMING'")

describe()
## year is a character so destring it
destring("year")


## generate emp*year for no reason
gen("empyear", "emp*year")

listif()

count()
count("emp <= 1000")
```

In vanilla R, the same, common data manipulations are a bit more cumbersome.

## Current State

I started writing the package on September 20, 2018.  The package is under active development, but is not yet ready for error-free use and it is not fully-documented.

The following functions alongside their Stata equivalents have been implemented so far:

1. `collapse` (Stata equivalent: `collapse`) produces summary statistics from a dataset.
2. `count` (Stata equivalent: `count`) counts how many observations satisfy some conditions.
3. `describe` (Stata equivalent: `describe`) lists variables in the dataset. Currently, it just gives the name, but I may add Stata-like documentation capabilities at some point.
4. `do` executes an arbitrary piece of R code using the active dataset.
5. `drop` (Stata equivalent: `drop`) drops variables or observations from the dataset. `keep` is also included.
6. `forvar` (Stata equivalent: `foreach var of varlist`) executes code for each variable in variable list.
7. `gen` (Stata equivalent: `generate` and `replace`) generates new variable or replaces variables that are transformations of other variables in the dataset.
8. `listif` (Stata equivalent: `list`) lists observations and variables that satisfy certain conditions.
9. `preserve` (Stata equivalent: `preserve`) preserves a data set as it is before modification.
10. `shape` (Stata equivalent: `reshape`) reshapes data from long to wide or from wide to long formats. Unlike other R packages that implement reshaping, the syntax is intuitive to people with experience using Stata.
11. `use` (Stata equivalent: `use`) uses a data set and puts it into memory.




