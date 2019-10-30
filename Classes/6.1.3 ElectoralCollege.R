#config
    library(tidyverse)
    library(dslabs)
    data(polls_us_election_2016)

#to predict the electoral college we start by aggregating the results from a week before the election
results <- polls_us_election_2016 %>%
    filter(state!="U.S." &
                    !grepl("CD", state) &
                    enddate >="2016-10-31" &
                    (grade %in% c("A+", "A", "A-", "B+") | is.na(grade))) %>%
    mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
    group_by(state) %>%
    summarize(avg = mean(spread), sd = sd(spread), n= n()) %>%
    mutate(state = as.character(state))

#the closest states are called battleground states
results %>% arrange(abs(avg))

#we now use left_join(), a function we learn more about later, to add the number of electoral votes
results <- left_join(results, results_us_election_2016, by = "state")
results %>% arrange(abs(avg))

#we now need to assign a SD to states that only had one poll, by taking the median of the other states
results <- results %>%
    mutate(sd = ifelse(is.na(sd),median(results$sd, na.rm=TRUE),sd))

#now we can create the Bayesian calculation that applies to each state
mu <- 0
tau <- 0.02
results %>% mutate(sigma = sd/sqrt(n),
                    B = sigma^2/(sigma^2+tau^2),
                    posterior_mean = B*mu + (1-B)*avg,
                    posterior_se = sqrt(1/(1/sigma^2+1/tau^2))) %>%
    arrange(abs(posterior_mean))

#and with this we can create the Monte Carlo to replicate 10,000 election night results
mu <- 0
tau <- 0.02
clinton_EV <- replicate(10000, {
    results %>% mutate(sigma = sd/sqrt(n),
                    B = sigma^2/(sigma^2+tau^2),
                    posterior_mean = B*mu + (1-B)*avg,
                    posterior_se = sqrt(1/(1/sigma^2+1/tau^2)),
                    simulated_result = rnorm(length(posterior_mean),posterior_mean,posterior_se),
                    clinton = ifelse(simulated_result>0, electoral_votes,0)) %>%
    summarize(clinton = sum(clinton)) %>%
    .$clinton + 7## 7 for Rhode Island and D.C.
})

mean(clinton_EV>269)

#now we canc reate a histogram for the outcome
data.frame(clinton_EV) %>%
    ggplot(aes(clinton_EV)) +
    geom_histogram(binwidth=1)+
    geom_vline(xintercept = 269)

#we didnt include the general bias, it was between 1-2%, because the election was so close in several states
#and the many polls created made the estimated standard error small, and pollsters over confident.
#here we simulate 3% general bias
mu <- 0
tau <- 0.02
bias_sd <- 0.03
clinton_EV <- replicate(10000, {
    results %>% mutate(sigma = sqrt(sd^2/n + bias_sd^2),
                    B = sigma^2/(sigma^2+tau^2),
                    posterior_mean = B*mu + (1-B)*avg,
                    posterior_se = sqrt(1/(1/sigma^2+1/tau^2)),
                    simulated_result = rnorm(length(posterior_mean),posterior_mean,posterior_se),
                    clinton = ifelse(simulated_result>0, electoral_votes,0)) %>%
    summarize(clinton = sum(clinton)) %>%
    .$clinton + 7## 7 for Rhode Island and D.C.
})

mean(clinton_EV>269)

#looking at the histogram we can see the variability added by introducing the general bias
data.frame(clinton_EV) %>%
    ggplot(aes(clinton_EV)) +
    geom_histogram(binwidth=1)+
    geom_vline(xintercept = 269)