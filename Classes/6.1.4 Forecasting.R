#config
    library(tidyverse)
    library(dslabs)
    data(polls_us_election_2016)

head(polls_us_election_2016)

#we are going to look at the variability of polls over time for one pollster
one_pollster <- polls_us_election_2016 %>%
    filter(pollster == "Ipsos" & state == "U.S.") %>%
    mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

#as there is no pollster effect, perhaps the theoretical standard error will match the data-derived standard deviation
se <- one_pollster %>%
    summarize(empirical = sd(spread),
                theoretical = 2*sqrt(mean(spread*1-mean(spread))/min(samplesize)))
se

#now we check the distribution of the data, to confirm that it isnt normal
one_pollster %>% ggplot(aes(spread)) +
    geom_histogram(binwidth = 0.01, color = "black")

#we can confirm this variability across several pollsters
#the geom_smooth() line is also an example of a trend f(t)
polls_us_election_2016 %>%
    filter(state =="U.S." & enddate >="2016-07-01") %>%
    group_by(pollster) %>%
    filter(n()>=10) %>%
    ungroup() %>%
    mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
    ggplot(aes(enddate, spread)) +
    geom_smooth(method = "loess", span = 0.1) +
    geom_point(aes(color = pollster), show.legend = FALSE, alpha = 0.6)

#the following shows trends for the 2 main candidates similar to how forecasters broadcast their trends
polls_us_election_2016 %>%
    filter(state =="U.S." & enddate >="2016-07-01") %>%
    select(enddate, pollster, rawpoll_clinton, rawpoll_trump) %>%
    rename(Clinton = rawpoll_clinton, Trump = rawpoll_trump) %>%
    gather(candidate, percentage, -enddate, -pollster) %>%
    mutate(candidate = factor(candidate, levels = c("Trump","Clinton"))) %>%
    group_by(pollster) %>%
    filter(n()>=10) %>%
    ungroup() %>%
    ggplot(aes(enddate, percentage, color=candidate)) +
    geom_point(show.legend = FALSE, alpha = 0.4) +
    geom_smooth(method = "loess", span = 0.15) +
    scale_y_continuous(limits = c(30,50))