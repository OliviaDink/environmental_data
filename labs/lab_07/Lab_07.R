##using apply
# Create simulated data
dat = matrix(1:49, nrow = 7, ncol = 7)
print(dat)

#see minimum and maximum numbers of each row 
apply(dat, MARGIN = 1, FUN = min)
apply(dat, MARGIN = 1, FUN = max)


#see mean value of each column 
apply(dat, MARGIN = 2, FUN = mean)


#load data
library(here)
moths = read.csv(here("data", "moths.csv"))
head(moths)


##calculating parametric CI with t distribution 
# Choose significance level
alpha = 0.05

# 2: Calculate sample standard error:
n = sum(!is.na(moths$anst))
sse = sd(moths$anst, na.rm = TRUE) / sqrt(n)

# 3: Calculate critical t-values:
t_crit = abs(qt(alpha / 2, df = n - 1))

# 4: Calculate the CI radius:
ci_radius = sse * t_crit

# The CI is the sample mean +/- the radius:
anst_ci = c(
  lower = mean(moths$anst) - ci_radius,
  upper = mean(moths$anst) + ci_radius)

print(round(anst_ci, 4))


##bootstrap Confidence Interval #non-parametric 
#create an empty vector 
m = 10000
# numeric() creates an vector of length m with all values initialized to zero
result = numeric(m)
head(result)

#perform the bootstrap 
for(i in 1:m)
{
  result[i] = mean(sample(moths$anst, replace=TRUE))
}

#calculate bootstrap mean 
mean(result)

#calculate quantiles
quantile(result,c(0.025,0.975))


##Bootstrap interval using boot()
install.packages("boot")
require(boot)
boot(data, statistic, R)
?boot

#vector fuction that exclude NAs
boot_mean = function(x, i)
{
  return(mean(x[i], na.rm = TRUE))
}

#bootstrap for 10000 interations using our function 
myboot = 
  boot(
    data = moths$anst,
    statistic = boot_mean,
    R = 10000)
print(myboot)

#check other attributes
str(myboot)

mean(moths$anst)
myboot$t0
mean(myboot$t) - myboot$t0
sd(myboot$t)


#extract bootstrap confidence interval 
quantile(
  myboot$t,
  c(0.025, 0.975))


###Setting up the bootstrap ##rarefraction start
#remove first column
moth_dat = moths[,-1]
head(moth_dat)

#make data more compact
n = nrow(moth_dat) #number of rows or sample observations
m = 100 #number of bootstrap iterations
moth_result = matrix(
  nrow = m,
  ncol = n)

#runnig the bootstrap simulation for species richness
n = nrow(moth_dat) #number of rows or sample observations
n
m = 100 #number of bootstrap iterations

moth_result = matrix(
  nrow = m,
  ncol = n)


# The outer loop: runs once for each bootstrap iteration.  index variable is i
for(i in 1:m)
{
  # The inner loop: simulates increasing sampling intensity
  # Sampling intensity ranges from 1 site to the complete count of sites (24)
  # index variable is j
  for(j in 1:n)
  {
    # sample the input data row indices, with replacement
    rows_j = sample(n, size = j, replace=TRUE)
    
    # Creates a new data matrix from the resampled rows.
    t1 = moth_dat[rows_j, ]
    
    # Calculates the column sums of the new data matrix.
    t2 = apply(t1, 2, sum)
    
    # Counts the number of columns in which any moths were observed
    moth_result[i, j] = sum(t2 > 0)
  }
}

head(moth_result)


##packaging code in a transferable function 
rarefaction_sampler = function(input_dat, n_iterations)
{
  n_input_rows = nrow(input_dat)
  
  results_out = matrix(
    nrow = n_iterations,
    ncol = n_input_rows)
  
  # The outer loop: runs once for each bootstrap iteration.  index variable is i
  for(i in 1:n_iterations)
  {
    # The inner loop: simulates increasing sampling intensity
    # Sampling intensity ranges from 1 site to the complete count of
    # sites in the input data (n)
    # index variable is j
    for(j in 1:n)
    {
      # sample the input data row indices, with replacement
      rows_j = sample(n, size = j, replace=TRUE)
      
      # Creates a new data matrix
      t1 = input_dat[rows_j, ]
      
      # Calculates the column sums
      t2 = apply(t1, 2, sum)
      
      # Counts the number of columns in which any moths were observed
      results_out[i, j] = sum(t2 > 0)
    }
  }
  return(results_out)
}

rarefact = rarefaction_sampler(moth_dat, 100)
head(rarefact)


##deleted and corrected code 
# This clears the current R session's environment
rm(list = ls())

# Re-read my data:
moths = read.csv(here("data", "moths.csv"))
moth_dat = moths[,-1]

rarefaction_sampler = function(input_dat, n_iterations)
{
  n_input_rows = nrow(input_dat)
  
  results_out = matrix(
    nrow = n_iterations,
    ncol = n_input_rows)
  
  # The outer loop: runs once for each bootstrap iteration.  index variable is i
  for(i in 1:n_iterations)
  {
    # The inner loop: simulates increasing sampling intensity
    # Sampling intensity ranges from 1 site to the complete count of
    # sites in the input data (n)
    for(j in 1:n)
    {
      # sample the input data row indices, with replacement
      rows_j = sample(n, size = j, replace=TRUE)
      
      # Creates a new data matrix
      t1 = input_dat[rows_j, ]
      
      # Calculates the column sums
      t2 = apply(t1, 2, sum)
      
      # Counts the number of columns in which any moths were observed
      results_out[i, j] = sum(t2 > 0)
    }
  }
  return(results_out)
}

rarefact = rarefaction_sampler(moth_dat, 100)



##Debugging tempplate/see if worked 
# This clears the current R session's environment
rm(list = ls())

# Re-read my data:
moths = read.csv(here("data", "moths.csv"))

rarefaction_sampler = function(input_dat, n_iterations)
{
  n_input_rows = nrow(input_dat)
  
  results_out = matrix(
    nrow = n_iterations,
    ncol = n_input_rows)
  
  # The outer loop: runs once for each bootstrap iteration.  index variable is i
  for(i in 1:n_iterations)
  {
    # The inner loop: simulates increasing sampling intensity
    # Sampling intensity ranges from 1 site to the complete count of
    # sites in the input data (n)
    for(j in 1:n_input_rows)
    {
      # sample the input data row indices, with replacement
      rows_j = sample(n_input_rows, size = j, replace=TRUE)
      
      # Creates a new data matrix
      t1 = input_dat[rows_j, ]
      
      # Calculates the column sums
      t2 = apply(t1, 2, sum)
      
      # Counts the number of columns in which any moths were observed
      results_out[i, j] = sum(t2 > 0)
    }
  }
  return(results_out)
}

rarefact = rarefaction_sampler(moths[,-1], 100)
head(rarefact)



##building the refraction curve 

# Re-read my data:
moths = read.csv(here("data", "moths.csv"))

rarefact = rarefaction_sampler(moths[,-1], 10000)

rare_mean = apply(rarefact, 2, mean)
rare_quant = apply(rarefact, 2, quantile, probs=c(0.025, 0.975))
rare = t(rbind(rare_mean, rare_quant))


##plotting the curve 
matplot(
  rare,
  type='l',
  xlab='Number of sampling plots',
  ylab='Species richness',
  col = c(1,2,4),
  main="Moth Rarefaction Curve")

legend(
  'bottomright',
  legend=c('mean','2.5%','97.5%'),
  lty=c(1,2,3),col=c(1,2,4), inset=c(.1,.1))







#######Questions 
require(palmerpenguins)


sse_mean = function(x)
{
  sse = sd(x, na.rm = TRUE)/sqrt(length(x) - sum(is.na(x)))
  return(sse)
}

dat_pen = subset(penguins, species == "Gentoo")

sse_mean(dat_pen$bill_length_mm)


#calculating the CI
# Choose significance level
alpha = 0.05

# 2: Calculate sample standard error:
n = sum(!is.na(dat_pen$bill_length_mm))
n

sse = sd(dat_pen$bill_length_mm, na.rm = TRUE) / sqrt(n)
sse

sd(dat_pen$bill_length_mm, na.rm = TRUE)

?sd

# 3: Calculate critical t-values:
t_crit = abs(qt(alpha / 2, df = n - 1))
t_crit 

t_crit = qt(c(0.025, 0.975), df = length(dat_pen$bill_length_mm)-1)
t_crit 


# 4: Calculate the CI radius:
ci_radius = sse * t_crit

# The CI is the sample mean +/- the radius:
anst_ci = c(
  lower = mean(dat_pen$bill_length_mm, na.rm = TRUE) - ci_radius,
  upper = mean(dat_pen$bill_length_mm, na.rm = TRUE) + ci_radius)

print(round(anst_ci, 4))

t.test(dat_pen$bill_length_mm) 


#Q6

m = 10000
result = numeric(m)
head(result)
  
for(i in 1:m)
{
  result[i] = mean(sample(dat_pen$bill_length_mm, replace=TRUE), na.rm = TRUE)
}
mean(result)
quantile(result,c(0.025,0.975))



#boot 
boot_mean = function(x, i)
{
  return(mean(x[i], na.rm = TRUE))
}

myboot = 
  boot(
    data = dat_pen$bill_length_mm,
    statistic = boot_mean,
    R = 10000)
print(myboot)


#Q8
sd(myboot$t)
  quantile(
    myboot$t,
    c(0.025, 0.975)
  )

  
#Q9

  
  rarefaction_sampler = function(input_dat, n_iterations)
  {
    n_input_rows = nrow(input_dat)
    
    results_out = matrix(
      nrow = n_iterations,
      ncol = n_input_rows)
    
    # The outer loop: runs once for each bootstrap iteration.  index variable is i
    for(i in 1:n_iterations)
    {
      # The inner loop: simulates increasing sampling intensity
      # Sampling intensity ranges from 1 site to the complete count of
      # sites in the input data (n)
      for(j in 1:n_input_rows)
      {
        # sample the input data row indices, with replacement
        rows_j = sample(n_input_rows, size = j, replace=TRUE)
        
        # Creates a new data matrix
        t1 = input_dat[rows_j, ]
        
        # Calculates the column sums
        t2 = apply(t1, 2, sum)
        
        # Counts the number of columns in which any moths were observed
        results_out[i, j] = sum(t2 > 0)
      }
    }
    return(results_out)
  }
  
  rarefact = rarefaction_sampler(moths[,-1], 100)
  head(rarefact)
  
  ##building the refraction curve 
  
  # Re-read my data:
  moths = read.csv(here("data", "moths.csv"))
  rarefact = rarefaction_sampler(moths[,-1], 10000)
  
  rare_mean = apply(rarefact, 2, mean)
  rare_quant = apply(rarefact, 2, quantile, probs=c(0.025, 0.975))
  rare = t(rbind(rare_mean, rare_quant))
  
  
  ##plotting the curve 
  matplot(
    rare,
    type='l',
    xlab='Number of sampling plots',
    ylab='Species richness',
    main="Olivia's Awesome Rarefaction Curve")
  
  legend(
    'bottomright',
    legend=c('mean','2.5%','97.5%'),
    lty=c(1,2,3),col=c(1,2,3), inset=c(.1,.1))
  

  
  
  



