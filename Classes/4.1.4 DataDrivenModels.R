#config
library(tidyverse)
library(dslabs)
data(polls_us_election_2016)
#take polls table from '4.1.3 PollDataBias.R'

#we collect the last-reported poll from each pollster
one_poll_per_pollster <- polls %>%
    group_by(pollster) %>%
    filter(enddate == max(enddate)) %>%
    ungroup
    
#create a histogram of the data
one_poll_per_pollster %>% 
    ggplot(aes(spread)) + geom_histogram(binwidth = 0.01)

#we can compute the standard deviation of the data using the sd() function
#NOTE: there is far more detail in the workbook
sd(one_poll_per_pollster$spread)

#we can now create a now confidence interval using our data-driven model
#NOTE: the word document contains all theory
results <- one_poll_per_pollster %>%
    summarize(avg = mean(spread), se = sd(spread)/sqrt(length(spread))) %>%
    mutate(start = avg - 1.96*se, end = avg + 1.96*se)
round(results*100,1)