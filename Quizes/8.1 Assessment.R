#config
    library(tidyverse)
    library(dslabs)
    options(digits =3)
    data(brexit_polls)

#actual parameters
    p <- 0.481 #official proportion voting "Remain"
    d <- 2*p-1 #official spread

#QUESTION 1: EXPECTED VALUE AND STANDARD ERROR OF A POLL
    #a: proportion of actual voters of Remain
        N <- 1500
        N*p

    #b: standard error of the actual voters of Remain
        se <- sqrt((p*(1-p))/N) 
        se*N

    #c: X_hat of the proportion of Remain
        X_hat <- p
        X_hat

    #d: SE_hat of the proporiton of Remain
        SE_hat <- sqrt((p*(1-p))/N)
        SE_hat

    #e: Expected value of d in actual numbers
        d

    #f: standard error of d
        2*sqrt((p*(1-p))/N) 

#QUESTION 2: ACTUAL BREXIT POLL ESTIMATES
    #config
        head(brexit_polls)

        brexit_polls <- brexit_polls %>%
            mutate(x_hat = (spread+1)/2)
    
    #a: average of the observed spreads
        d_hat <- mean(brexit_polls$spread)

    #b: standard deviation of the observed spreads
        se_d_hat <- sd(brexit_polls$spread)
        se_d_hat

    #c: average x_hat
        mean(brexit_polls$x_hat)

    #d: standard deviation of x_hat
        sd(brexit_polls$x_hat)

#QUESTION 3: CONFIDENCE INTERVAL OF A BREXIT POLL
    #relevant field
        X_hat <- brexit_polls[1,10]
        SE_hat <- sqrt(X_hat*(1-X_hat)/brexit_polls[1,5])

    #a: lower bound 95% confidence interval
        (X_hat-qnorm(0.975)*SE_hat)

    #b: lower bound 95% confidence interval
        (X_hat+qnorm(0.975)*SE_hat)

#QUESTION 4: CONFIDENCE INTERVALS FOR POLLS IN JUNE
    #config
        june_polls <- brexit_polls %>%
            filter(enddate > "2016-06-01") %>%
            mutate(x_hat = (spread+1)/2,
                    se_x_hat = sqrt(x_hat*(1-x_hat)/samplesize),
                    lower = (x_hat - qnorm(0.975)*se_x_hat)-.5,
                    upper = (x_hat + qnorm(0.975)*se_x_hat)-.5,
                    hit_0 = lower <= 0 & upper >= 0,
                    hit_remain = lower > 0,
                    hit_actual = 2*lower < d & 2*upper > d)

        head(june_polls)

    #a: how many polls in june?
        length(june_polls)

    #b: proportion of polls with confidence interval that covers 0
        mean(june_polls$hit_0)

    #c: proportion of polls with confidence interval that predicts remain (>0.5)
        mean(june_polls$hit_remain)

    #d: proportion of polls with confidence interval that predicts leave (<0.5)
        mean(june_polls$hit_actual)

#QUESTION 5: HIT RATE BY POLLSTER
    #a: group and summarize by pollster
        q5_polls <- june_polls %>%
            group_by(pollster) %>%
            summarize(hit=mean(hit_actual),polls=n()) %>%
            arrange(desc(hit))

        q5_polls

#QUESTION 6: BOXPLOT OF BREXIT POLLS BY POLL TYPE
        june_polls %>%
            group_by(poll_type) %>%
            ggplot() +
            geom_boxplot(aes(poll_type,spread))

#QUESTION 7: COMBINED SPREAD ACROSS POLL TYPE
    #config
        q7_polls <- june_polls %>%
            group_by(poll_type) %>%
            summarize(N = sum(samplesize),
                        spread = sum(spread*samplesize)/N,
                        p_hat = (spread+1)/2,
                        se_p_hat = 2*sqrt(p_hat*(1-p_hat)/N),    
                        lower = (spread - (qnorm(0.975)*se_p_hat)),
                        upper = (spread + (qnorm(0.975)*se_p_hat)))
        q7_polls
    
    #a: lower bound of the 95% confidence interval of online voters
        q7_polls$lower

    #b: upper bound of the 95% confidence interval of online voters
        q7_polls$upper

#QUESTION 8: INTERPRETING COMBINED SPREAD ESTIMATES
    #a: q7_polls interpretation of confidence intervals

#QUESTION 9: CHI-SQUARED P-VALUE
    #config
        brexit_hit <- brexit_polls %>%
            mutate(p_hat = (spread + 1)/2,
                    se_spread = 2*sqrt(p_hat*(1-p_hat)/samplesize),
                    spread_lower = spread - qnorm(.975)*se_spread,
                    spread_upper = spread + qnorm(.975)*se_spread,
                    hit = spread_lower < -0.038 & spread_upper > -0.038) %>%
            group_by(poll_type, hit) %>%
            summarize(n=n()) %>%
            spread(poll_type,n)

        brexit_hit

    #a: p-value of brexit_hit
        brexit_hit[,2] %>% chisq.test() #online
        brexit_hit[,3] %>% chisq.test() #telephone

        brexit_hit %>% chisq.test()

#QUESTION 10: ODDS RATIO OF ONLINE AND TELEPHONE
    #config
        brexit_hit <- brexit_polls %>%
            mutate(p_hat = (spread + 1)/2,
                    se_spread = 2*sqrt(p_hat*(1-p_hat)/samplesize),
                    spread_lower = spread - qnorm(.975)*se_spread,
                    spread_upper = spread + qnorm(.975)*se_spread,
                    hit = spread_lower < -0.038 & spread_upper > -0.038) %>%
            group_by(poll_type, hit) %>%
            summarize(n=n()) %>%
            spread(poll_type,n)

        brexit_hit$Online[2]

    #a: odds an online poll generates a confidence interval that covers hit
        on_odds <- (brexit_hit$Online[2]/sum(brexit_hit$Online))/
                    (brexit_hit$Online[1]/sum(brexit_hit$Online))
        on_odds        

    #b: odds an telephone poll generates a confidence interval that covers hit
        tel_odds <- (brexit_hit$Telephone[2]/sum(brexit_hit$Telephone))/
                    (brexit_hit$Telephone[1]/sum(brexit_hit$Telephone))
        tel_odds   

    #c odds ration
        on_odds/tel_odds

#QUESTION 11: PLOTTING SPREAD OVER TIME
    #a: plot the spread over time
        brexit_polls %>%
            ggplot(aes(enddate,spread,col=poll_type)) +
            geom_point() +
            geom_hline(aes(yintercept = -0.038)) +
            geom_smooth(method = "loess", span = 0.4)

#QUESTION 12: PLOTTING RAW PERCENTAGES OVER TIME
    #config
        brexit_long <- brexit_polls %>%
            gather(vote,proportion,"remain":"undecided") %>%
            mutate(vote = factor(vote))

        head(brexit_long)

    #a: create and analyse the graph for proportion over time
        brexit_long %>%
            ggplot(aes(enddate,proportion, col=vote)) +
            geom_point() +
            geom_smooth(method = "loess", span = 0.3)