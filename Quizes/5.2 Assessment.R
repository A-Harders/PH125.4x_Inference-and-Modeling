#config
    library(tidyverse)
    options(digits = 3)

#Setting Variables
    Pr_InA <- 0.2
    Pr_InB <- 0.6
    Pr_InC <- 0.15
    Pr_InD <- 0.05
    Pr_ntFound_inArea <- 0.1
    Pr_ntFound_ntArea <- 1

#question 1 - Pr_B questions
    #a - probability plane not in B
        1-Pr_InB

    #b - probability plane in B but not found
        Pr_InB*Pr_ntFound_inArea

    #c - probability plane is not found on day 1
        Pr_NotB_Day1 <- (1-Pr_InB)+(Pr_InB*Pr_ntFound_inArea)

#question 2 - Bayes Theory equation, no question to answer here

#question 3 - Posterior probability for each grid location
    #a - probability plane is in B if it isnt found on day 1
        (Pr_ntFound_inArea*Pr_InB) / Pr_NotB_Day1

    #b - probability plane is in C if it isnt found on day 1
        (Pr_ntFound_ntArea*Pr_InC) / Pr_NotB_Day1

    #c - which has the highest posterior probability and should be serached on day 2?
        Bayes_day2 <- (Pr_ntFound_ntArea*Pr_InA) / Pr_NotB_Day1 # winner
        (Pr_ntFound_ntArea*Pr_InC) / Pr_NotB_Day1
        (Pr_ntFound_ntArea*Pr_InD) / Pr_NotB_Day1

#question 4 - reporting probabilities before search
    #a - probability of finding plane on day 1
        day1 <- (1-Pr_ntFound_inArea)*Pr_InB

    #b - likelihood of finding it day 2
        day2 <- (Bayes_day2*0.9)*(1-day1)

    #c - probability of finding the plane in the first 2 days
        day1+day2