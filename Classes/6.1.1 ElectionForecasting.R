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

#here we can calculate the distribution of d which is our best guess given we have had no input from polling data
mu <- 0
tau <- 0.035
sigma <- results$se
Y <- results$avg
B <- sigma^2/(sigma^2+tau^2)

posterior_mean <- B*mu + (1-B)*Y
posterior_mean
posterior_se <- sqrt(1/((1/sigma^2)+(1/tau^2)))
posterior_se

#as the posterior distribution is normal we can make the prbability statement using the credible interval
posterior_mean + c(-1.96,1.96)*posterior_se

#we can also compute the probability that d is bigger than 0
1-pnorm(0,posterior_mean,posterior_se)