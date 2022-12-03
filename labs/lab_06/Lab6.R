?sd()

#create fuction
sse_mean = function(x) sd(penguins$bill_depth_mm, na.rm = TRUE)/sqrt(length(penguins$bill_depth_mm))

#calculate standard error mean
require(palmerpenguins)
penguins
sse_mean(penguins$bill_depth_mm)





#boxplot of 3
boxplot(
  flipper_length_mm ~ species, data = penguins,
  ylab = "Flipper length (mm)")

#boxplot of 2
dat_pen = subset(penguins, species != "Gentoo")
boxplot(
  flipper_length_mm ~ species, data = dat_pen,
  ylab = "Flipper length (mm)")

#remove unused factor levels 
dat_pen = droplevels(subset(penguins, species != "Gentoo"))
{
  par(mfrow = c(1, 2))
  boxplot(
    flipper_length_mm ~ species, data = penguins,
    ylab = "Flipper length (mm)")
}


#resampling with replacement 
# for reproducibility
set.seed(123)

flipper_shuffled = sample(
  penguins$flipper_length_mm, replace = TRUE)

{
  par(mfrow = c(1, 2))
  boxplot(
    flipper_length_mm ~ species, data = penguins,
    ylab = "Flipper length (mm)",
    main = "Original Data")
  boxplot(
    flipper_shuffled ~ penguins$species,
    ylab = "Flipper length (mm)",
    main = "MonteCarlo Resampled Data",
    xlab = "species")
}


#Bootstrap resampling with replacement
penguins2 = penguins[sample(1:nrow(penguins), replace = T), ]

{
  par(mfrow = c(1, 2))
  boxplot(
    flipper_length_mm ~ species, data = penguins,
    ylab = "Flipper length (mm)",
    main = "Original Data")
  boxplot(
    flipper_length_mm ~ species, data = penguins2,
    ylab = "Flipper length (mm)",
    main = "Bootstrap Data")
}


#repeated Monte Carlo sampling
par(mfrow = c(4, 4), mar = c(1, 1, 1, 1))
for (i in 1:16)
{
  
  flipper_shuffled = sample(
    penguins$flipper_length_mm, replace = TRUE)
  
  boxplot(
    flipper_shuffled ~ penguins$species,
    ann = F, axes = F)
  box()
  
}

#Classical t.test
t.test(dat_pen$flipper_length_mm ~ dat_pen$species)

#t.test resampling with replacement 
# Reset the random number generator state for reproduceablility
set.seed(1)
flipper_shuffled = sample(dat_pen$flipper_length_mm)
boxplot(flipper_shuffled ~ dat_pen$species) #plot data

#rerun t.test on reshuffled data
t_test_1 = t.test(flipper_shuffled ~ dat_pen$species)
t_test_1

#t.test is great to see differene in means between two groups
t_test = t.test(dat_pen$flipper_length_mm ~ dat_pen$species)
t_test

#show means of groups
t_test$estimate

#calculate difference in menas 
diff_observed = round(diff(t_test$estimate), digits = 3)
print(diff_observed, digits = 3)

#calculate difference in means with aggregate function
agg_means = aggregate(
  flipper_length_mm ~ species, 
  data = dat_pen, 
  FUN = "mean", 
  na.rm = TRUE)
diff_observed = diff(agg_means[, 2])

agg_means
diff_observed


#sample sizes #show number of individuals of each species 
table(dat_pen$species)


#resampling with replacement is same thing as radnomly sampling 68 flipper lenghts in one and 152 in other
n_1 = 68
n_2 = 152

dat_1 = sample(dat_pen$flipper_length_mm, n_1, replace = TRUE)
dat_2 = sample(dat_pen$flipper_length_mm, n_2, replace = TRUE)

diff_simulated = 
  mean(dat_1, na.rm = TRUE) - mean(dat_2, na.rm = TRUE)

#difference in means for resampled data 
print(c(observed = diff_observed, simulated = diff_simulated))

#simulation function 
x = dat_pen$flipper_length_mm #same as before
n_1 = 68
n_2 = 152

dat_1 = sample(x, n_1, replace = TRUE)
dat_2 = sample(x, n_2, replace = TRUE)

diff_simulated = 
  mean(dat_1, na.rm = TRUE) - mean(dat_2, na.rm = TRUE)


two_group_resample_diff = function(x, n_1, n_2) #function
{
  x_ok = x[!is.na(x)]
  
  dat_1 = sample(x_ok, n_1, replace = TRUE)
  dat_2 = sample(x_ok, n_2, replace = TRUE)
  
  diff_simulated = 
    mean(dat_1, na.rm = TRUE) - mean(dat_2, na.rm = TRUE)
  return(diff_simulated)
}

#output
set.seed(54321)
two_group_resample_diff(dat_pen$flipper_length_mm, 68, 152)

#resampling experiment 
n =2000
mean_differences = c()
for (i in 1:n)
{
  mean_differences = c(
    mean_differences,
    two_group_resample_diff(dat_pen$flipper_length_mm, 68, 152)
  )
}
hist(mean_differences)


sum(abs(mean_differences) >= diff_observed)

#Retrieving named elements
t_test = t.test(flipper_shuffled ~ dat_pen$species)
str(t_test)

t_test$estimate



#Q1
sse_mean(penguins$body_mass_g)
sse_mean(mtcars$mpg)

sse_mean = function(x) sd(penguins$body_mass_g, na.rm = TRUE)/sqrt(length(penguins$body_mass_g))
sse_mean(penguins$body_mass_g)

sse_mean = function(x) sd(mtcars$mpg, na.rm = TRUE)/sqrt(length(mtcars$mpg)
sse_mean(mtcars$mpg)



#Q1                     

rm(list = ls())
sse_mean = function(x)
{
  sse = sd(x, na.rm = TRUE)/sqrt(length(x) - sum(is.na(x)))
  return(sse)
}

sum(is.na(penguins$body_mass_g) #example
    
    sse_mean(penguins$body_mass_g)
    sse_mean(mtcars$mpg)
    
    
#Q2
   
 two_group_resample_diff = function(x, n_1, n_2) 
{

     x_ok = x[!is.na(x)]
     
    x1 = sample(x_ok, n_1, replace = TRUE)
    x2 = sample(x_ok, n_2, replace = TRUE)
     
     difference_in_means = 
       mean(x1, na.rm = TRUE) - mean(x2, na.rm = TRUE)
     return(difference_in_means)
   }
     
 set.seed(54321)
 two_group_resample_diff(dat_pen$flipper_length_mm, 68, 152)
 
 
#Q4+5
 
 #calculate if shuffle all data 
 n = 1000
 mean_differences = c()
 for (i in 1:n)
 {
   mean_differences = c(
     mean_differences,
     two_group_resample_diff(dat_pen$body_mass_g, 68, 152)
   )
 }
 hist(mean_differences)
    
#how many means would be over 5.8
sum(abs(mean_differences) > 5.8)


#Q7
boxplot
dat_pen = droplevels(subset(penguins, species != "Gentoo"))
boxplot(
  body_mass_g ~ species, data = dat_pen,
  ylab = "Body mass (g)")

agg_means = aggregate(
  body_mass_g ~ species, 
  data = dat_pen, 
  FUN = "mean", 
  na.rm = TRUE)
diff_crit = diff(agg_means[,2])

agg_means
diff_crit

t.test(dat_pen$body_mass_g~ dat_pen$species)


sum(abs(mean_differences)> diff_crit)


x = dat_pen$body_mass_g
n_1 = 68
n_2 = 152

dat_1 = sample(x, n_1, replace = TRUE)
dat_2 = sample(x, n_2, replace = TRUE)

diff_simulated = 
  mean(dat_1, na.rm = TRUE) - mean(dat_2, na.rm = TRUE)

hist(mean_differences)

