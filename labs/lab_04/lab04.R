# Generate a vector of x-values
x = seq(-3, 3, length.out = 1000)
y = dnorm(x)

plot(x, y, main = "Normal PDF", type = "l")
abline(h = 0)

# Calculates the value of y for a linear function, given the coordinates
# of a known point (x1, y1) and the slope of the line.# Calculates the value of y for a linear function, given the coordinates
# of a known point (x1, y1) and the slope of the line.
line_point_slope = function(x, x1, y1, slope)
{
  get_y_intercept = 
    function(x1, y1, slope) 
      return(-(x1 * slope) + y1)
  
  linear = 
    function(x, yint, slope) 
      return(yint + x * slope)
  
  return(linear(x, get_y_intercept(x1, y1, slope), slope))
}

#use it
set.seed(123)
n = 17
slope = 0.7
intcp = 0.2

guess_x = 6
guess_y = 4
guess_slope = 0.72

x = runif(n = n, min = 1, max = 10)
y = rnorm(n = n, mean = slope * x + intcp)

plot(x, y, pch = 16)
curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = T)

#create data
n_pts = 10
x_min = 1
x_max = 10

# X values are uniformly distributed
x_random = runif(n = n_pts, min = x_min, max = x_max)

# Y values are normally-distributed.
# I used the default parameters for mean and sd.
y_random = rnorm(n = n_pts)

dat_random = data.frame(x = x_random, y = y_random)

plot(y ~ x, data = dat_random, pch = 8)


##fit determinsitic function
guess_x = 6
guess_y = 0
guess_slope = 0.1

plot(y ~ x, data = dat_random, pch = 8)
curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = T)


#add predicted values 
line_point_slope(dat_random$x, guess_x, guess_y, guess_slope)

#calculate residuals and #create new column
dat_random$y_predicted <- line_point_slope(dat_random$x, guess_x, guess_y, guess_slope)
head(dat_random)


dat_random$resids = y_random - dat_random$y_predicted 
head(dat_random)
sum(dat_random$resids)  

abs(dat_random$resids)


plot(x=dat_random$y_predicted, y=dat_random$resids)
hist(x=dat_random$resids)



#QUESTIONS 1

norm_17 = rnorm(n = 17, mean = 10.4, sd =2.4)
norm_30 = rnorm(n = 30, mean = 10.4, sd = 2.4)
norm_300 = rnorm(n = 300, mean = 10.4, sd = 2.4)
norm_3000 = rnorm(n = 3000, mean = 10.4, sd = 24)

par(mfrow= c(2,2))

hist(norm_17)
hist(norm_30)
hist(norm_300)
hist(norm_3000)

#question 2
#saving as png
require(here)
png(
  filename = here("lab_04_hist_01.png"),
  width = 1500, height = 1600, 
  res = 180, units = "px")

dev.off() #save file 

#7
# Generate a vector of x-values

require(here)
svg(
  filename = here("norm_1.svg"))

x = seq(-20, 20, length.out = 1000)
y = dnorm(x, mean = 10.4, sd = 2.4)


plot(x, y, main = "PDF with mean= 10.4 and sd = 2.4", type = "l", xlim = c(0, 20))
abline(h = 0)

dev.off()

#8
require(here)
png(
  filename = here("plots"),
  width = 1500, height = 1600, 
  res = 180, units = "px")


set.seed(36)
n_pts = 20 #different number of points 
x_min = 5 #different x and y values
x_max = 10

# X values are uniformly distributed
x_random = runif(n = n_pts, min = x_min, max = x_max)

# Y values are normally-distributed.
# I used the default parameters for mean and sd.
y_random = rnorm(n = n_pts)

dat_random = data.frame(x = x_random, y = y_random)

par(mfrow= c(2,2))

plot(x = x_random, y = y_random, main = "Random Scatterplot")
hist(y_random, main = "random histogramm y_random")
hist(x_random, main ="random histogramm x_value")
boxplot(y_random, main= "random histogramm y_value")

dev.off() #save file 
#put png code before and dev.off code at last > safes all in between


use curve function and add values > value n between so 7.5 for x and 0 for y and o for slope because it is not steep

#11

require(here)
png(
  filename = here("plot with line"),
  width = 1500, height = 1600, 
  res = 180, units = "px")

plot(x = x_random, y = y_random, main = "Random Scatterplot with fitted linear function")

#parameters
guess_x = 7.5
guess_y = 0
guess_slope = 0


curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = T)
dev.off() #save file


#13
require(here)
png(
  filename = here("random data plots"),
  width = 1500, height = 1600, 
  res = 180, units = "px")
head(dat_random)

#calculate residuals and #create new column
set.seed(36)
n_pts = 20 #different number of points 
x_min = 5 #different x and y values
x_max = 10

# X values are uniformly distributed
x_random = runif(n = n_pts, min = x_min, max = x_max)

# Y values are normally-distributed.
# I used the default parameters for mean and sd.
y_random = rnorm(n = n_pts)

dat_random = data.frame(x = x_random, y = y_random)

dat_random$y_predicted <- line_point_slope(dat_random$x, guess_x, guess_y, guess_slope)
head(dat_random)


dat_random$resids = y_random - dat_random$y_predicted 
head(dat_random)
sum(dat_random$resids)  

par(mfrow= c(2,2))
plot(x=dat_random$y_predicted, y=dat_random$resid, main = "Scatter predicted values and residuals")
hist(x=dat_random$resids, main = "Histogram of residuals")

dev.off() #save file




