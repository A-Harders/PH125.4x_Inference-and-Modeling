#the majority of the maths is in the word document
#we can compute the probability based on the estimate of the standard error
X_hat <- 0.48
se <- sqrt(X_hat*(1-X_hat)/25)

#we can now use this estimated se to compute the probability using pnorm
pnorm(0.01/se) - pnorm(-0.01/se)

#had we used a sample size of 1000 instead of 25 we return a far more accurate result
se2 <- sqrt(X_hat*(1-X_hat)/1000)

pnorm(0.01/se2) - pnorm(-0.01/se2)