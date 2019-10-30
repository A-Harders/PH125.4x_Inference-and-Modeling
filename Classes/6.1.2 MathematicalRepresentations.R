#config
    library(tidyverse)
    library(dslabs)
    data(polls_us_election_2016)

    #variable config
    polls <- polls_us_election_2016 %>%
        filter(state == "U.S." & enddate >= "2016-10-31" &
                (grade %in% c("A+", "A", "A-", "B+") | is.na(grade))) %>%
        mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

    one_poll_per_pollster <- polls %>% group_by(pollster) %>%
        filter(enddate == max(enddate)) %>%
        ungroup()

    results <- one_poll_per_pollster %>%
        summarize(avg = mean(spread), se = sd(spread)/sqrt(length(spread))) %>%
        mutate(start = avg - 1.96*se, end = avg + 1.96*se)

#here we are building the collected data model, simulating several poll data points from the same pollster
J <- 6
N <- 2000
d <- 0.21
p <- (d+1)/2
X <- d + rnorm(J,0,2*sqrt(p(1-p)/N))

#now if we have data points from different pollsters we need to represent this with 2 indices
#to do so we need to define a function and use sapply()
I <- 5
J <- 6
N <- 2000
d <- 0.21
p <- (d+1)/2
X <- sapply(1:I, function(i){
    d + rnorm(J,0,2*sqrt(p*(1-p)/N))
})

#now we need to account for the pollster effect, we do this for each pollster and then add the espilon after
#hi is an assumption of standard error 0.025 per pollster, is common to all observed spreads from a specific pollster
I <- 5
J <- 6
N <- 2000
d <- 0.21
p <- (d+1)/2
h <- rnorm(I,0,0.025)
X <- sapply(1:I, function(i){
    d + h[i] + rnorm(J,0,2*sqrt(p*(1-p)/N))
})

#now that we understand the maths, we can rewrite the code from 6.1.1 and show that we get a result much closer to FiveThirtyEight
mu <- 0
tau <- 0.035
sigma <- sqrt(results$se^2 + 0.025^2)
Y <- results$avg
B <- sigma^2/(sigma^2+tau^2)

posterior_mean <- B*mu + (1-B)*Y
posterior_se <- sqrt(1/(1/sigma^2+1/tau^2))

1- pnorm(0, posterior_mean, posterior_se)