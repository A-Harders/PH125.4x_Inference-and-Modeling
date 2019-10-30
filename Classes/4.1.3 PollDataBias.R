#config
library(tidyverse)
library(dslabs)
data(polls_us_election_2016)

#filter the polls to include only the national polls within a week of the election, and with a grade of b or less
polls <- polls_us_election_2016 %>%
    filter(state =="U.S." & enddate >= "2016-10-31" &
            (grade %in% c("A+","A","A-","B+") | is.na(grade)))

#now we add the spread estimate
polls <- polls %>%
    mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

#we compute the entire election day spread based on all of the polls
#when we do this we get an exceptionally small margin of error because of how large the sample size is
d_hat <- polls %>%
    summarize(d_hat = sum(spread*samplesize)/sum(samplesize)) %>%
    .$d_hat

#now we create the estimated probability and margin of error from the estimated spread
p_hat <- (d_hat+1)/2
moe <- 1.96*2*sqrt(p_hat*(1-p_hat)/sum(polls$samplesize))

#now we can report our spread of 1.43% with moe 0.66%
d_hat
moe

#election comes and goes and the actual percentage is 2.1%
#we create a histogram to see why our estimate was incorrect
polls %>%
    ggplot(aes(spread)) +
    geom_histogram(color="black",binwidth=0.01)
    #the data isnt normal, and is way outside our margin of error

#we try and see the variation between pollsters by numbr of polls and the spread
polls %>%
    filter(n() >=6) %>%
    group_by(pollster) %>%
    ggplot(aes(pollster, spread)) +
    geom_point() +
    theme(axis.text.x = element_text(angle = 90,hjust =1))
    #we see wildly varying results

#now we check the standard error pollster by pollster
polls %>%
    group_by(pollster) %>%
    filter(n() >= 6) %>%
    summarize(se = 2*sqrt(p_hat * (1-p_hat) / median(samplesize)))
    #interestingly we see similar standard errors across polls

#these difference are referred to as house effects, we also call them pollster bias