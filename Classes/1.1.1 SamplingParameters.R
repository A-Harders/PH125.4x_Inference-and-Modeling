#config
library(tidyverse)
library(dslabs)

#dslabs has a quick poll taking ability
ds_theme_set()
take_poll(25)

#To take the sigma from the spread (2x-1) you must multiply by 2
N <- 25
p <- 0.45
sigma <- 2*sqrt(p*(1-p)/N)
sigma