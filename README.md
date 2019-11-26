# genvar: an R package for imperative data manipulation and regression (like Stata)

This is the README for this version of `genvar`. It may not apply to the version of `genvar` on CRAN. See the README file on the CRAN page, https://cran.rstudio.com/web/packages/genvar/.

## Installation

`genvar` is now on CRAN! To install latest CRAN release, run the following from R:

```R
install.packages("genvar", dependencies=TRUE)
```

To install latest development version from Github, run the following from R:
```R
install.packages("devtools", dependencies=TRUE)
library(devtools)
install_github("flynnzac/genvar", ref="master")
library(genvar)
```

## Motivation
The goal of this package is to remove one barrier to using R, a free software statistical package, for researchers in the social sciences who are used to Stata's data model and imperative syntax.  Stata assumes a rectangular model for data (there are observations and variables) while R allow for more flexible data structures. Stata also uses an imperative language where commands intentionally modify the state of the dataset while R uses a more function-based syntax. There are advantages to R's additional flexibility, but in the social sciences, data is almost always in the (observation, variable) framework, the Stata way of working with data is ingrained, and the additional flexibility of R can make things that are routine in Stata more difficult because the user has to know a much wider variety of functions to get the desired result.  This package solves the problem by implementing a Stata-like method for manipulating data in R so that this data modification approach (which I will call the "imperative" approach because it involves issuing commands to modify state) is available in a free software package. 

The package implements an environment where there is one active dataset and commands can be used to modify or reference variables from that dataset by issuing "commands" as opposed to R's standard environment (applying functions to objects and returning values).

`genvar` also uses R regression packages (`plm`, `sandwich`, and `clubSandwich`) which incorporate panel regression, robust and clustered standard errors, time series operators, and fixed effects all into one estimation command (this is mostly tying together other R packages which use a more function-object interface into an imperative interface).  The goal is not just to replicate Stata's environment, but to offer an improved imperative data environment that takes advantage of the additional flexibility of R.

To get a feel for what `genvar` looks like see the example in examples/test.r. The syntax is more intuitive (well, hopefully) than standard R to people who are used to thinking in the Stata data model and its imperative language.

## Bug Reporting

Report any bugs or feature requests  (always willing to add features that you would like to be ported to this environment in R) to the Github repo https://github.com/flynnzac/genvar/issues.

See below for the basic concepts and the reference manual for a list of commands.

# Unique `genvar` variable types

## Variable lists

Variable lists in `genvar` are specified by quoting the names of variables like, `"educ wage black"`. The names can be specified using wildcard characters as well. For example, if the variables "x100 x2 x3" make up the dataset, they can be all be included by specifying, `"x*"`. If we only want to list "x2 x3", then we can specify `"x?"` because `?` matches only one character.

## Quoted Expressions

Many `genvar` commands work by using "quoted expressions" which are bits of code provided as arguments to the function that could not be executed in the current R environment but will be properly processed in `genvar`.  They can optionally be enclosed in quotation marks when necessary. For example, to use `genvar`'s `gen` command to generate log wages, you might type `gen(lnwage, log(wage))`. If you want to list the variables of the form `x1,x2,x3,...`, you would use `describe("x*")` to avoid R interpreting the `x*` as an incomplete multiplication expression.

# Basic overview of currently available functions

Use the `use` function to load a dataset into the `genvar` environment. 

Then, modify the dataset or add additional transformations of variables with the `gen` command.

Analyze the data using `summarize`, `reg` (for linear regression), `logit` or `probit` (for binary regression), or execute arbitrary `R` code in the `genvar` environment with the `do` command (if anyone writes a package that makes use of this interface to add new commands, let me know!).  You can create a dataset of summary statistics with `collapse`.

There is support for panel data using `xtset` (`reg` works for panel regression as well, see its manual page) and `L` can be used to generate lags and leads of variables in the panel.

The data can be reshaped from long-to-wide or from wide-to-long using the `shape` command.

`forvar` can apply code by variable to a certain variable list in the dataset.

Let me know if you have any feature requests!

# Examples

Check out the `examples` folder for examples. `test.r` shows most of the features.


