#propose we want to build a simulation to test our tools, and ensure they work
B <- 10000
N <- 1000
X_hat <- replicate(B,{
    X <- sample(c(0,1), size=N, replace=TRUE, prob=c(1-p,p))
    mean(X)
})

#we build the below but we dont have p
#we can just pick a series of p values to simulate against those
p <- 0.45

#with this simulated value now we can take the proportion and sd results
mean(X_hat)
sd(X_hat)

#histograms and q-q plots confirm that the normal approximation is accurate
library(gridExtra)
library(tidyverse)
p1 <- data.frame(X_hat=X_hat) %>% ggplot(aes(X_hat)) +
    geom_histogram(binwidth = 0.005, color="black")
p2 <- data.frame(X_hat=X_hat) %>% ggplot(aes(sample=X_hat)) +
    stat_qq(dparams = list(mean=mean(X_hat),sd=sd(X_hat))) +
    geom_abline() +
    ylab("X_hat") +
    xlab("Theoretical normal")
grid.arrange(p1,p2,nrow=1)
