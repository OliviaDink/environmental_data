install.packages("psych")
require(psych)

pairs.panels(iris)

#pick out some columns
names(iris)
pairs.panels(iris[, c("Sepal.Length", "Sepal.Width", "Petal.Length")])

install.packages("here")
require(here)


read.csv(here("data", "my_data.csv"))
require(here)

#load data set
dat_bird = read.csv(
  here("data", "bird.sta.csv")
)
head(dat_bird)
 

dat_hab = read.csv(
  here("data", "hab.sta.csv")
)
head(dat_hab)

#check if have same dimensions
dim(dat_bird)
dim(dat_hab)


head(dat_bird)
head(dat_hab)

#batot = basal area 
dat_all = merge(dat_bird, dat_hab)
plot(ba.tot ~ elev, data = dat_all)


sample(dat_all$CEWA, 100)
sum(dat_all$CEWA)
dat_all$CEWA == TRUE


my_vec = rep(1:3, 5)
my_vec == 3

my_vec > 1

my_vec = dat_all

dat_all$CEWA>=1
dat_all$CEWA 


cewa_present_absent <- as.numeric(dat_all$CEWA >=1)

plot(x = dat_all$elev, y = cewa_present_absent)



get_logistic_param_a = function(slope, midpoint)
{
  b = slope / 4
  return (-midpoint * (slope / 4))
}

# Function to calculate the logistic parameter b given the slope
get_logistic_param_b = function(slope)
{
  return (slope / 4)
}


# Calculate the value of the logistic function at x, given the parameters a and b.
logistic = function(x, a, b)
{
  val = exp(a + b * x)
  return(val / (1 + val))
}

# Calculate the value of the logistic function at x, given a slope and midpoint.
logistic_midpoint_slope = function(x, midpoint, slope)
{
  b = get_logistic_param_b(slope)
  a = get_logistic_param_a(slope, midpoint)
  return(logistic(x, a, b))
}




plot(x = dat_all$elev, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = 0.1), add = TRUE)


#negative slope 

plot(x = dat_all$elev, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = -0.1), add = TRUE)


#shallower 
plot(x = dat_all$elev, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = -0.005), add = TRUE)

names(dat_all)
names(dat_bird)

plot(x = dat_all$elev, y = cewa_present_absent)



pairs.panels(dat_all[, c("slope", "aspect", "elev", "ba.tot")])

apply(dat_bird[, -(1:3)] , 2, sum) 

cbch
wiwa








#Choose two bird species and create plots of presence/absence (on the y-axis) and basal area (on the x axes).

sample(dat_all$CBCH, 100)
sample(dat_all$WIWA, 100)

CBCH <- as.numeric(dat_all$CBCH >=1)
WIWA <- as.numeric(dat_all$WIWA >=1)
plot(x = dat_all$ba.tot, y = CBCH)

#logistic function plot
WIWA_absence_pres <- as.numeric(dat_all$WIWA >= 1)
plot(x = dat_all$elev, y = WIWA_absence_pres, xlab = "Total Basal Area of Hardwoods", ylab = "Likelihood WIWA presence and absence", col=adjustcolor(col = 1, 0.4), pch=16)

title("Likelihood of occurence for species WIWA")

curve(logistic_midpoint_slope(x, midpoint = 100, slope = 0.1), add = TRUE)

--- 
CBCH_absence_pres <- as.numeric(dat_all$CBCH >= 1)
plot(x = dat_all$elev, y = CBCH_absence_pres, xlab = "Total Basal Area", ylab = "Likelihood CBCH presence and absence", col=adjustcolor(col = 1, 0.4), pch=16)

title("Likelihood of occurence for species CBCH")

curve(logistic_midpoint_slope(x, midpoint = 70, slope = 0.1), add = TRUE)



#how many gray jay in total 

sum(dat_all$GRJA)

    
q <- dat_all$GRJA>=1
q
sum(q)


