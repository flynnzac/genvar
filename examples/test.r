rm(list=ls(all=TRUE))
library(genvar)
clear()
library(plm)
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
listif()

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
use(Produc, clear=TRUE)

r = reg("emp", "unemp")
r

xtset(timevar="year", obsvar="state")
r = reg("emp", "unemp", hac="year")
r
r = reg("emp", "unemp", cluster="year")
r
p = preserve()
keepvar("state year emp unemp")

## add some garbage data to show addobs, not real data
addobs("state='Puerto Rico',year=1990,emp=100,unemp=0.05")
fillin("state year")

r = reg("emp", "unemp", effect="twoways", cluster="year")
r

restore(p)

gen("empmedian", "emp > median(emp)")

r = logit("empmedian", "unemp")

data = getdata()

