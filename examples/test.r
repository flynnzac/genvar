rm(list=ls(all=TRUE))
library(rata)

use(cars)
listif()
p <- preserve()
collapse(~mean(dist)|speed)
listif()
listif("speed <= 10 | dist > 50")

restore(p)
listif()

keep(~speed)
listif()

restore(p)
gen(avgspeed~, fun="speed/dist")
listif()
