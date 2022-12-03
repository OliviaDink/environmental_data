##Lab5

#make function
ricker_fun = function(x, a, b) 
{
  return(a * x * exp(-b * x))
}

#create curve
curve(
  ricker_fun(x, 1, 1), 
  from = 0, to = 5, add = FALSE, 
  main = "Ricker function: a = 1, b = 1",
  ylab = "f(x)", xlab = "x")


#create exponential functional 
exp_fun = function(x, a, b) 
{
  return(a * b * exp(-b * x))
}

curve(
  exp_fun(x, 1, 1), 
  from = 0, to = 5, add = FALSE, 
  main = "Exponential function: a = 1, b = 1",
  ylab = "f(x)", xlab = "x")


curve(
  exp_fun(x, 2.2, 1/15), add = FALSE, from = 0, to = 50,
  ann = FALSE, axes = TRUE, ylab = "f(x)"); box()



#simulated data on line
# Seed the RNG so we can reproduce our results
set.seed(1234567)

# Specify the x-range and number of points:
n_pts = 50
x_min = 2
x_max = 10

# Generate the x-values
x_sim = runif(n_pts, min = x_min, max = x_max)


param_intercept = 2.3
param_slope = 0.67
y_pred = param_intercept + x_sim * param_slope
plot(x_sim, y_pred, main = "Simulated Data\nNo Errors", xlab = "", ylab = "")


#error 1
error_mean = 0
error_sd = 0.25

y_observed = 
  y_pred + 
  rnorm(
    n = n_pts, 
    mean = error_mean, 
    sd = error_sd)
plot(x_sim, y_observed, main = "Normally Distributed Errors\n Constant Variance", xlab = "", ylab = "")


#error 2
error_mean = 0
error_sd = 0.1

y_observed_2 = 
  y_pred + 
  rnorm(
    n = n_pts, 
    mean = error_mean, 
    sd = error_sd * x_sim)

par(mfrow = c(1, 2))
plot(x_sim, y_observed, main = "Normally Distributed Errors\n Constant Variance", xlab = "", ylab = "")
plot(x_sim, y_observed_2, main = "Normally Distributed Errors\n Increasing Variance", xlab = "", ylab = "")

#thrid graph 
error_mean = 0
error_sd = 0.1

n_pts = 50
x_min = 2
x_max = 10

n= 50

y_observed_3  = 
  y_pred + 
  rnorm(
    n=n_pts, 
    mean = error_mean, 
    sd = rexp(n, rate= 1.2))

par(mfrow = c(1, 3))
plot(x_sim, y_observed, main = "Normally Distributed Errors\n Constant Variance", xlab = "", ylab = "")
plot(x_sim, y_observed_2, main = "Normally Distributed Errors\n Increasing Variance", xlab = "", ylab = "")
plot(x_sim, y_observed_3, main = "Normally Distributed Errors\n Increasing Variance", xlab = "", ylab = "")

#fitted linear model
fit_1 = lm(y_observed ~ x_sim)
fit_2 = lm(y_observed_2 ~ x_sim)
fit_3 = lm(y_observed_3 ~ x_sim)

par(mfrow = c(1, 3))

plot(y_observed ~ x_sim); abline(fit_1)
plot(y_observed_2 ~ x_sim); abline(fit_2)
plot(y_observed_3 ~ x_sim); abline(fit_3)


#Q1
require(here)
salamander= read.csv(here("data", "dispersal.csv"))


salamander


Q2:
  #create exponential functional 
  exp_fun = function(x, a, b) 
  {
    return(a * exp(-b * x))
  }

curve(
  exp_fun(x, 1, 1), 
  from = 0, to = 5, add = FALSE, 
  main = "Exponential function: a = 1, b = 1",
  ylab = "f(x)", xlab = "x")


curve(
  exp_fun(x, 2.2, 1/15), add = FALSE, from = 0, to = 50,
  ann = FALSE, axes = TRUE, ylab = "f(x)"); box()


#add curve1

a = 1.9
b = 0.1

curve(
  exp_fun(x, a, b), add = FALSE, from = 0, to = 40,
  ann = FALSE, axes = TRUE, ylab = "f(x)"); box()


#curve 2
curve(
  exp_fun(x, 1.9, 0.3), add = TRUE, from = 0, to = 40,
  ann = FALSE, axes = TRUE, ylab = "f(x)", lty = 2, lwd = 1); box()


#curve 3
curve(
  exp_fun(x, 1.2, 0.2), add = TRUE, from = 0, to = 40,
  ann = FALSE, axes = TRUE, ylab = "f(x)", col=2); box()


#curve4
curve(
  exp_fun(x, 1.2, 0.4), add = TRUE, from = 0, to = 40,
  ann = FALSE, axes = TRUE, ylab = "f(x)", col= "red"); box()

##ricker function Q5-7

ricker_fun = function(x, a, b) 
{
  return(a * x * exp(-b * x))
}

#curve1
curve(
  ricker_fun(x, 25, 0.2), 
  from = 0, to = 5, add = FALSE, 
  main= "ricker function",
  ylab = "f(x)", xlab = "x", 
  ylim = c(0,100))

#curve 2
curve(
  ricker_fun(x, 20, 0.2), 
  from = 0, to = 5, add = TRUE,
  ylab = "f(x)", xlab = "x", lty = 2, lwd = 1)

#curve 3
curve(
  ricker_fun(x, 10, 0.2), 
  from = 0, to = 5, add = TRUE,
  ylab = "f(x)", xlab = "x", lty = 2, lwd = 1)


#curve 4
curve(
  ricker_fun(x, 75, 0.3), 
  from = 0, to = 5, add = TRUE,
  ylab = "f(x)", xlab = "x", col= "red")


#curve 5
curve(
  ricker_fun(x, 50, 0.3), 
  from = 0, to = 5, add = TRUE,
  ylab = "f(x)", xlab = "x", col= "red", lty = 2, lwd = 1)


#curve 6
curve(
  ricker_fun(x, 40, 0.3), 
  from = 0, to = 5, add = TRUE,
  ylab = "f(x)", xlab = "x", col= "red", lty = 2, lwd = 1)


#Uestion 9

library(here)

dat_dispersal = read.csv(here("data", "dispersal.csv"))

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

plot(
  dat_dispersal$dist.class,
  dat_dispersal$disp.rate.ftb,
  xlim = c(0, 1500),
  xlab = "distance class", 
  ylab = "standardized dispersal rate", 
  main = "Marbled Salamander - first time breeders")

curve(line_point_slope(x, 800, 0.1, -0.0003), add = TRUE)

##exponential 
exp_fun = function(x, a, b) 
{
  return(a  * exp(-b * x))
}

curve(
  exp_fun(x, 0.8, 0.003), add = TRUE, from = 0, to = 1500,
  ann = FALSE, axes = TRUE, ylab = "f(x)"); box()

#ricker 
ricker_fun = function(x, a, b) 
{
  return(a * x * exp(-b * x))
}

#create curve
curve(
  ricker_fun(x, 0.015 , 0.01), 
  from = 0, to = 1500, add = TRUE, 
  main = "Ricker function: a = 1, b = 1",
  ylab = "f(x)", xlab = "x")


#Questionlast


resids_linear
resids_exp
resids_ricker



dat_random$y_predicted <- line_point_slope(dat_random$x, guess_x, guess_y, guess_slope)
head(dat_random)


dat_random$resids_linear = y_random - dat_random$y_predicted 
head(dat_random)
sum(dat_random$resids)  

par(mfrow= c(2,2))
hist(x=dat_random$resids, main = "Histogram of residuals")


par(mfrow= c(3,1))

#predicted linear
predicted <- line_point_slope(dat_dispersal$dist.class, 800, 0.1, -0.0003)

dat_dispersal$residslinear <- dat_dispersal$disp.rate.ftb - predicted 

hist(x = dat_dispersal$residslinear, main = "linear residuals", xlab = "residuals")


#predicted exponential 

predicted2 <- exp_fun(dat_dispersal$dist.class, 0.8, 0.003)

dat_dispersal$residsexpo <- dat_dispersal$disp.rate.ftb - predicted2
hist(x = dat_dispersal$residsexpo, main = "exponential residuals")

#predicted ricker 

predicted3 <- ricker_fun(dat_dispersal$dist.class, 0.015, 0.01)

dat_dispersal$residsricker <- dat_dispersal$disp.rate.ftb - predicted3
hist(x=dat_dispersal$residsricker)
