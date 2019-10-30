#explaining bayes theorem from the word document by using a monte carlo
#the prupose of this is to visualise Bayes' theorem
prev <- 0.00025
N <- 100000
outcome <- sample(c("Disease","Healthy"), N, replace = TRUE, prob = c(prev,1-prev))
N_D <- sum(outcome =="Disease")
N_H <- sum(outcome =="Healthy")
N_D
N_H

#observing the results we see that the people with the disease is exceptionally low
#this makes the probability that we see some false positives quite high
#there are so many people getting the test, that, although its rare, we are going to get a few getting a positive test despite being healthy
#so we test all of our subjects with the appropriate test accuracy
accuracy <- 0.99
test <-vector("character",N)
test[outcome=="Disease"] <- sample(c("+","-"), N_D, replace=TRUE, prob = c(accuracy, 1-accuracy)) #99% accuracys with a positive test
test[outcome=="Healthy"] <- sample(c("-","+"), N_H, replace=TRUE, prob = c(accuracy, 1-accuracy)) #99% accuracys with a negative test

#we can make a table of our healthy and diseased people and their test results to show how many false positives we received
table(outcome, test)

#if we then take the correct positive results and divide it by the total positive results we find how we came to ~2% with our Bayesian analysis
21/(1046+21)