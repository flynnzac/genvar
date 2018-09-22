rm(list=ls(all=TRUE))
library(rata)
data(Produc)

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
