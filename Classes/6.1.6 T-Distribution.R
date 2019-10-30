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

#we can use the t-distribution to account for the variability introduced by sigma - detailed explanation in workbook
z <- qt(0.975, nrow(one_poll_per_pollster)-1)
one_poll_per_pollster %>%
    summarize(avg = mean(spread), moe = z*sd(spread)/sqrt(length(spread))) %>%
    mutate(start = avg - moe, end = avg + moe)

one_poll_per_pollster %>%
    summarize(avg = mean(spread), moe = qnorm(0.975)*sd(spread)/sqrt(length(spread))) %>%
    mutate(start = avg - moe, end = avg + moe)