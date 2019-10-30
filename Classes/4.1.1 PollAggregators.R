library(tidyverse)

#by running a monte carlo of 12 polls, we can simulate 12 polls taken in the 2012 presidential election and show how aggregators work
d <- 0.039
Ns <- c(1298, 533, 1342, 897, 774, 254, 812, 324, 1291, 1056, 2172, 516)
p <- (d+1)/2

confidence_intervals <- sapply(Ns, function(N) {
    X <- sample(c(0,1), size = N, replace = TRUE, prob=c(1-p,p))
    X_hat <- mean(X)
    SE_hat <- sqrt(X_hat*(1-X_hat)/N)
    2*c(X_hat, X_hat - 2*SE_hat, X_hat + 2*SE_hat)-1
})

polls <- data.frame(poll=1:ncol(confidence_intervals),
                    t(confidence_intervals),
                    sample_size = Ns)
names(polls)<-c("poll","estimate","low","high","sample_size")
polls

#as we dont have the raw poll data we construct an estimate to reconstruct 1 large poll
d_hat <- polls %>%
    summarize(avg = sum(estimate*sample_size) / sum(sample_size)) %>%
    .$avg

#now that we have an estimate of d, we construct an estimate for the proportion voting for Obama
#which we then use for standrd error and margin of error
p_hat <- (1+d_hat)/2
moe <- 2*1.96*sqrt(p_hat*(1-p_hat)/sum(polls$sample_size))
moe

#by using the weighted average, we can predict the spread
round(d_hat*100,1)
round(moe*100,1)