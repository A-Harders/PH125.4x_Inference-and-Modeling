#config
library(tidyverse)
library(dslabs)
data("nhtemp")

#here is an example of geom_smooth() using confidence intervals
data.frame(year = as.numeric(time(nhtemp)), temperature = as.numeric(nhtemp)) %>%
    ggplot(aes(year, temperature)) +
    geom_point() +
    geom_smooth(col="red",fill = "blue") +
    ggtitle("Average Yearly Temperatures in New Haven")

#Monte Carlo to simulate how the random variable changes for confidence interval mathematics
#the results change everytime
p <- 0.45
N <- 1000
X <- sample(c(0,1), size=N, replace = TRUE, prob = c(1-p,p))
X_hat <- mean(X)
SE_hat <- sqrt(X_hat*(1-X_hat)/N)
c(X_hat - 2*SE_hat, X_hat + 2*SE_hat)

#how to get change the confidence intervals is through using qnorm()
z <- qnorm(0.995)
z

#by definition qnorm(0.995) is 0.995
#and by symetry 1-qnorm(0.995) is 1-0.995
#and we compute this using pnorm minus pnorm and get 99%
pnorm(z) - pnorm(-z)

#we can use this approach for any percentile q

#NOTE: for exactly 95% we should be using a number slightly smaller than 2
qnorm(0.975)

#we can create a monte carlo to show that a 95% confidence interval includes p 95% of the time
B <- 10000
p <- 0.45
N <- 1000
inside <- replicate(B,{
    X <- sample(c(0,1), size=N, replace = TRUE, prob=c(1-p,p))
    X_hat <- mean(X)
    SE_hat <- sqrt(X_hat*(1-X_hat)/N)
    between(p,X_hat - 2*SE_hat, X_hat + 2*SE_hat)
})
mean(inside)