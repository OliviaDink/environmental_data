#data 
library(here)
dat_habitat <- read.csv(here("data", "hab.sta.csv"))
dat_bird <- read.csv(here("data", "bird.sta.csv"))
dat_all = merge(dat_bird, dat_habitat)

x_observed = c(2, 6)
print(x_observed)

#dpois 
dpois(x = 2, lambda = 4.5)
dpois(x = 6, lambda = 4.5)

#likelihood together
dpois(x = 2, lambda = 4.5) * dpois(x = 6, lambda = 4.5)

#vectorizing 
wiwa_counts = c(2, 6)
dpois(x = wiwa_counts, lambda = 4.5)


#product of counts 
prod(dpois(x = wiwa_counts, lambda = 4.5))

#sum of look lieklihood 
sum(log(dpois(x = wiwa_counts, lambda = 4.0)))

#numerical data exploration 
summary(dat_all$WIWA)

#graphical exploration 
hist(dat_all$WIWA)

#with breaks 
hist(dat_all$WIWA, breaks = 7)

#right one
hist(dat_all$WIWA, breaks = 0:7 - .5)


#if we dont know the max values ahead 
par(mfrow = c(1, 2))
dat = dat_all$WIWA
hist(dat, breaks = 0:(max(dat) + 1) - 0.5, main = "Histogram of\nWilson's Warbler counts")

dat = dat_all$GRJA
hist(dat, breaks = 0:(max(dat) + 1) - 0.5, main = "Histogram of\nGray Jay counts")

#warblers lieklihood 
sum(log(dpois(x = dat_all$WIWR, lambda = 1.46)))
   
hist(dat_all$WIWR, breaks = 0:7 - .5)  

#question 1

set.seed(1) 
vec_rnorm = rnorm(n = 10, mean = 0, sd = 0.1)
sum(log(dnorm(vec_rnorm, mean=0,sd=0.1)))










     
     
     
     
     










