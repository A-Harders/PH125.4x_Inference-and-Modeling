#config
library(tidyverse)

#if we ran a poll of 100,00 theory would tell us that we could predict the election perfectly, and we simulate this
N <- 100000
p <- seq(0.35,0.65,length=100)
SE <- sapply(p, function(x) 2*sqrt(x*(1-x)/N))
data.frame(p=p, SE=SE) %>% ggplot(aes(p,SE)) +
    geom_line()