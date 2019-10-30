#config
library(tidyverse)
library(dslabs)
data("research_funding_rates")

#7.1.1 - ASSOCIATION TESTS
#we re looking at the funding rates between males and females in the netherlands for research
#is the funding rate due to natural variance or is there an underlying issue with female funding rates
totals <- research_funding_rates %>%
    select(-discipline) %>%
    summarize_all(funs(sum)) %>%
    summarize(yes_men = awards_men,
                no_men = applications_men - awards_men,
                yes_women = awards_women,
                no_women = applications_women - awards_women)

totals %>% summarize(percent_men = yes_men/(yes_men+no_men),
                    percent_women = yes_women/(yes_women+no_women))

#fisher exact test results are usually displayed in a table like the below
#known as two-by-two tables
tab <- matrix(c(3,1,1,3),2,2)
rownames(tab)<-c("Poured Before","Poured After")
colnames(tab)<-c("Guessed before","Guessed after")
tab

#there is a fisher.test() function that performs the inference calculations
#it can be applied to two-by-two tables
fisher.test(tab, alternative="greater")

#7.1.2 - CHI-SQUARED TEST
#we can use the chi-squared test, which is similar to the fishers exact test, for our funding findings
#we have seen the women are ~15% and men have funding rate of ~ 18%
totals %>% summarize(percent_men = yes_men/(yes_men+no_men),
                    percent_women = yes_women/(yes_women+no_women))

#we need to computer the overall funding rate
funding_rate <- totals %>%
    summarize(percent_total = 
                    (yes_men +yes_women)/
                    (yes_men+no_men+yes_women+no_women)) %>%
    .$percent_total
funding_rate

#we now need to create a 2-by-2 table 
two_by_two <- tibble(awarded = c("no","yes"),
                    men = c(totals$no_men, totals$yes_men),
                    women = c(totals$no_women, totals$yes_women))
two_by_two

#the whole concept of chi-squared is to compare the 2-by-2 table to the overall rate, which we do below
tibble(awarded = c("no","yes"),
        men = c(totals$no_men, totals$yes_men) *
            c(1-funding_rate, funding_rate),
        women = c(totals$no_women, totals$yes_women) *
            c(1-funding_rate, funding_rate))

#we can run the chisq.test() function to quickly compute a 2-by-2 table and return the results
two_by_two %>%
    select(-awarded) %>%
    chisq.test()

#we can also run summary statistics on our 2-by-2
odds_men <- (two_by_two$men[2] / sum(two_by_two$men))/
            (two_by_two$men[1] / sum(two_by_two$men))

odds_women <- (two_by_two$women[2] / sum(two_by_two$women))/
              (two_by_two$women[1] / sum(two_by_two$women))

odds_men
odds_women