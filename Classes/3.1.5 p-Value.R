#the p-value shows how likely we are to see a spread or p this large if the null hypothesis is true
#the mathematics and explaination are in the workbook
N <- 100
z <- sqrt(N)*(0.02/0.05) #spread of 0.02
1-(pnorm(z) - pnorm(-z))

#alternaitve solution of z
z <- (0.02-0)/se