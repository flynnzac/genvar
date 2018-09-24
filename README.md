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

To get a feel for what `rata` looks like see the following example,
```R
rm(list=ls(all=TRUE))
library(rata)
data(Produc)

## "use" loads the data set Produc into memory
use(Produc)
listif()

## preserve data set
p = preserve()

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

## test out regression
use(Produc)

## Regression with heteroskedastic robust standard error
r = reg("emp", "unemp", robust=TRUE)
r

## Regression with heteroskedastic and auto-correlated standard errors

## Set time variable
xtset(timevar="year")

r = reg("emp", "unemp", hac="andrews")
r
```

In vanilla R, the same, common data manipulations are a bit more cumbersome.

## Current State

I started writing the package on September 20, 2018.  The package is under active development, but is not yet ready for error-free use and it is not fully-documented.

The end goal is to develop a "command-on-data" environment for R and to develop a more complete regression package for common models that incorporates robust and clustered standard errors, time series operators, and fixed effects all into one estimation command (this is mostly tying together other R packages which use a function-object interface into a command-on-data interface).  The goal is not just to replicate Stata's environment, but to offer an improved command-on-data environment that takes advantage of the additional flexibility of R.

