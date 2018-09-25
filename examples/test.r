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
collapse("sum(emp)", "year")

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

r = reg("emp", "unemp")
r

xtset(timevar="year")
r = reg("emp", "unemp", hac="andrews")
r

keepvar("state year emp unemp")

addobs("state=NA,year=1990,emp=NA,unemp=NA")
