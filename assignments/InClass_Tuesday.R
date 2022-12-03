dpois(x = 7, lambda = 10.4)

# Standard normal has mean = 0 and sd = 1
dnorm(0.5, mean = 0, sd = 1)

dnorm(1, mean = 0, sd = 1)

pnorm(0.5, mean = 0, sd = 1)


# How many points?
n = 13

# Create a vector of x-values from -4 to 4:
x = seq(from = -6, to = 6, length.out = n)

# Create the corresponding y-values:
y = dnorm(x, mean = 0, sd = 1)

# plot!
plot(y ~ x, type = "l")


# How many points?
n = 1000

# Create a vector of x-values from -4 to 4:
x = seq(from = -6, to = 6, length.out = n)

# Create the corresponding y-values:
y = dnorm(x, mean = 0, sd = 1)

# plot!
plot(y ~ x, type = "l", ylab = "Probability Density")


#use different mean and sd
y_2 = dnorm(x, mean = 0, sd = 2)
y2 = dnorm(x, mean = -2, sd= 1)


par(mfrow= c(1,2))


plot(y ~ x, type = "l", ylab = "Probability Density")
points(y_2 ~ x, type = "l", lty  = 2)
points(y2 ~ x, type = "l", ylab = "Probability Density", lty  = 2)


#Cumulative density plot 
y_cdf_1 = pnorm(x, mean = 0, sd = 1)
plot(y_cdf_1 ~ x, type = "l", ylab = "cumulative density")
y_cdf3 = pnorm(x, mean= -2, sd=1)

y_cdf_2 = pnorm(x, mean = 0, sd = 2)
plot(y_cdf_1 ~ x, type = "l", ylab = "Cumulative Density")
points(y_cdf_2 ~ x, type = "l", lty = 2)
plot()



x_bin = 0:5
y_bin_2 = dbinom(x_bin, size = 5, prob = 0.4)

barplot(
  height = y_bin_2,
  # the names to print with each bar:
  names.arg = x_bin,
  # Tells R to remove space between bars:
  space = 0,
  ylab = "Pr(x)",
  main = "Binomial: n = 5, p = 0.4")




# Q2
  
?dbinom
dbinom(0:6, size = 6, prob = 4/6)


dbinom(x=4, size=6, prob = 2/3)

#Q3


dbinom(x=0, size=7, prob = 2/3)

pbinom(q= 4, size = 6, prob = 2/3, lower.tail = FALSE, log.p = FALSE)


false = 4 or more 
true = 4 or less 

?pbinom
?pnorm


pnorm(q=1, mean = 0, sd= 1)

pnorm(q=1:2, mean = 0, sd= 1)

0.9772499 - 0.8413447



x_bin = 0:5 
y_bin_2 = dbinom(x_bin, size = 6, prob = 2/3) 

barplot( 
  height = y_bin_2, 
  # the names to print with each bar: 
  names.arg = x_bin, 
  # Tells R to remove space between bars: 
  space = 0, 
  ylab = "Pr(x)", 
  main = "Binomial: n = 6, p = 2/3") 







