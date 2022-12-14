require(here)
dat_bird <- data.frame(read.csv(here("data", "bird.sub.csv")))
dat_habitat <- data.frame(read.csv(here("data", "hab.sub.csv")))

birdhab = merge(dat_bird, dat_habitat, by.x=c("basin", "sub"))
dim(birdhab)
birdhab 


##Simulating Sample Size (lab 11)
sample_sizes = seq(2, 20)
alpha = 0.05
n_sims = 30

p_vals = numeric(n_sims)

sample_size_powers = numeric(length(sample_sizes))

max_x = max(birdhab$ls)

fit_1 = lm(BRCR ~ ls, data=birdhab)

fit_1_coefs = coefficients(fit_1)
fit_1_coefs
str(fit_1_coefs)

fit_1_summary = summary(fit_1)
str(fit_1_summary)
sd_obs = fit_1_summary$sigma
slope_obs = 0.005840474 
int_obs = 0.099103949

plot(
  birdhab$ls, birdhab$BRCR, 
  xlab = "late-successional forest extent",
  ylab = "Brown Creeper abundance",
  pch = 19)

points(
  x = birdhab$ls, 
  y = linear_simulator(
    x = birdhab$ls,
    y_int = int_obs,
    slope = slope_obs,
    st_dev = sd_obs
  ),
  col = adjustcolor("red", alpha = 0.3),
  pch = 16)


linear_sim_fit = function(x, slope, y_int, st_dev)
{
  y_sim = linear_simulator(
    x = x,
    y_int = y_int,
    slope = slope,
    st_dev = st_dev
  )
  fit_sim = lm(y_sim ~ x)
  return(fit_sim)
}

linear_simulator = function(x, y_int, slope, st_dev) 
{
  third= linear(x, y_int, slope)
  stoch = rnorm(n = length(x), mean = 0, sd= st_dev)
  return(third + stoch)
}

linear = function(x, y_int, slope) 
{
  first = (y_int + x * slope)
  
  return(first)
}

linear(x = 1, y_int = 1, slope = 1)
linear(x = 3:5, y_int = 1, slope = 1)
linear(x = 3:5, y_int = -1, slope = 1)
linear(x = 3:5, y_int = -1, slope = 0.01)


for(j in 1:length(sample_sizes))
{
  # A sequence of equally-spaced x-values:
  x_vals = seq(0, max_x, length.out = sample_sizes[j])
  
  for(i in 1:n_sims)
  {
    fit_sim = linear_sim_fit(
      x = x_vals,
      y_int = int_obs,
      slope = slope_obs,
      st_dev = sd_obs
    )
    p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
  }
  sample_size_powers[j] = sum(p_vals < alpha) / n_sims
}

sim_sample_size = 
  data.frame(
    sample_size = sample_sizes,
    power = sample_size_powers)

plot(
  power ~ sample_size, data = sim_sample_size,
  type = 'l', xlab = 'Sample size', ylab = 'Power')
abline(v = nrow(birdhab), lty = 2, col = 'red')

##Fitting LOWESS model

fit_lowess_50 = loess(power ~ sample_size, data = sim_sample_size, span = 0.5)

newdata_sample_size = data.frame(sample_size = seq(2, 20, length.out = 100)) 

plot(
  x = newdata_sample_size$sample_size,
  y = predict(fit_lowess_50, newdata = newdata_sample_size),
  type = "l",
  ylab = "Statistical Power", xlab = "Sample Size")

##Adding points
points(power ~ sample_size, data = sim_sample_size, pch = 20, col = "BLUE")

?legend()

legend("bottomright", legend = c("smoothed", "original"), lty = c(1, NA), pch = c(NA, 20), col = c("black", "blue"))

##Lab 5 code

dat_dispersal <- data.frame(read.csv(here("data", "dispersal.csv")))

plot(x= dat_dispersal$disp.rate.ftb, y=dat_dispersal$dist.class)

ricker_fun = function(x, a, b) 
{
  return(a * x * exp(-b * x))
}

plot(
 dat_dispersal$dist.class,
 dat_dispersal$disp.rate.ftb,
  xlim = c(0, 1500),
  xlab = "distance class", 
  ylab = "standardized dispersal rate", 
  main = "Marbled Salamander - first time breeders")


curve(
  ricker_fun(x, 0.015, 0.01), 
  from = 0, to = 1500, add = TRUE, 
  ylab = "f(x)", xlab = "x")

##Fitting a NLS model

fit_ricker_nls = nls(
  disp.rate.ftb ~ ricker_fun(dist.class, a, b),
  data = dat_dispersal,
  start = list(b = 0, a = 1))
summary(fit_ricker_nls)

plot(
  dat_dispersal$dist.class,
  dat_dispersal$disp.rate.ftb,
  xlim = c(0, 1500),
  xlab = "distance class", 
  ylab = "standardized dispersal rate", 
  main = "Marbled Salamander - first time breeders")

dist_newdata = data.frame(dist.class = seq(0, 1600, length.out = 1600))  

lines(predict(fit_ricker_nls, newdata = dist_newdata))
legend("topright", legend = c("nls fit", "ricker fit"), lty = c("solid", "dotted"), col = c("black", "red"))

curve(
  ricker_fun(x, 0.015, 0.01), 
  from = 0, to = 1500, add = TRUE, 
  ylab = "f(x)", xlab = "x",
  lty = "dotted", col = "red")

##Logistic Regression

dat_bird = read.csv(here("data", "bird.sta.csv"))
dat_habitat = read.csv(here("data", "hab.sta.csv"))
dat_all = merge(dat_bird, dat_habitat)

dat_all$HEWA_pres = dat_all$HEWA > 0
dat_all$HEWA_pres

plot(x= dat_all$elev, y= dat_all$HEWA_pres)

##Binary outcome data

# Hermit warbler presence/absence
dat_all$HEWA_pres = dat_all$HEWA > 0

# Create model fits
fit_hewa_slope = glm(HEWA_pres ~ slope, data = dat_all, family = binomial)
fit_hewa_ba_tot = glm(HEWA_pres ~ ba.tot, data = dat_all, family = binomial)
fit_hewa_both_additive = glm(HEWA_pres ~ slope + ba.tot, data = dat_all, family = binomial)
fit_hewa_both_interactive = glm(HEWA_pres ~ slope * ba.tot, data = dat_all, family = binomial)

summary(fit_hewa_both_additive)

n = 500

slope_newdata = data.frame(
  slope = seq(
    from = min(dat_all$slope, na.rm = T),
    to = max(dat_all$slope, na.rm = T),
    length.out = n
  )
)

ba_newdata = data.frame(
  ba.tot = seq(
    from = min(dat_all$ba.tot, na.rm = T),
    to = max(dat_all$ba.tot, na.rm = T),
    length.out = n))

##Model predictions

slope_newdata$hewa_predicted = 
  predict(
    fit_hewa_slope,
    newdata = slope_newdata,
    type = "response"
  )

ba_newdata$hewa_predicted = 
  predict(
    fit_hewa_ba_tot,
    newdata = ba_newdata,
    type = "response"
  )

##Plotting models 1

par(mfrow = c(2, 1))

# Presence/absence data, translucent points:
plot(
  HEWA_pres ~ slope, data = dat_all,
  xlab = "Percent Slope",
  ylab = "HEWA presence/absence",
  pch = 16, cex = 1.5, col = gray(0, 0.2)
)

lines(hewa_predicted ~ slope, data = slope_newdata)

plot(
  HEWA_pres ~ ba.tot, data = dat_all,
  xlab = "Basal Area",
  ylab = "HEWA presence/absence",
  pch = 16, cex = 1.5, col = gray(0, 0.2)
)

lines(hewa_predicted ~ ba.tot, data = ba_newdata)

##Plot both parameters (AIC)

AIC(
  fit_hewa_ba_tot,
  fit_hewa_slope,
  fit_hewa_both_additive,
  fit_hewa_both_interactive)

##Data set-up

n = 50

ba.tot = seq(
  from = min(dat_all$ba.tot, na.rm = T),
  to = max(dat_all$ba.tot, na.rm = T),
  length.out = n)
slope = seq(
  from = min(dat_all$slope, na.rm = T),
  to = max(dat_all$slope, na.rm = T),
  length.out = n)

new_dat_all = expand.grid(
  ba.tot = ba.tot,
  slope = slope)
head(new_dat_all)

tail(new_dat_all)

new_dat_all$pred_add = predict(
  fit_hewa_both_additive,
  newdata = new_dat_all,
  type = "response")

new_dat_all$pred_int = predict(
  fit_hewa_both_interactive,
  newdata = new_dat_all,
  type = "response")

##Add z axis
z_hewa_add = matrix(
  new_dat_all$pred_add,
  nrow = length(ba.tot),
  byrow = FALSE)

z_hewa_int = matrix(
  new_dat_all$pred_int,
  nrow = length(ba.tot),
  byrow = FALSE)

##3D plot

require(rgl)

##additive
rgl::persp3d(
  x = ba.tot,
  y = slope,
  z = z_hewa_add,
  col = "steelblue",
  xlab = "Basal Area",
  ylab = "Slope",
  zlab = "Pr(present)",
  alpha = 0.4)

rglwidget()

##interactive
persp3d(
  x = ba.tot,
  y = slope,
  z = z_hewa_int,
  col = "red",
  xlab = "Basal Area",
  ylab = "Slope",
  zlab = "Pr(present)",
  alpha = 0.4)

rglwidget()

##Contour version
par(mfrow = c(1, 2))
contour(
  x = ba.tot, y = slope,
  z = z_hewa_add,
  xlab = "Total Basal Area",
  ylab = "Percent Slope",
  main = "Additive")
contour(
  x = ba.tot,
  y = slope,
  z = z_hewa_int,
  xlab = "Total Basal Area",
  ylab = "Percent Slope",
  main = "Interactive")

##Question 1

fit_lowess_30 = loess(power ~ sample_size, data = sim_sample_size, span = 0.3)

newdata_sample_size = data.frame(sample_size = seq(2, 20, length.out = 100)) 

plot(
  x = newdata_sample_size$sample_size,
  y = predict(fit_lowess_30, newdata = newdata_sample_size),
  type = "l",
  ylab = "Statistical Power", xlab = "Sample Size",
  main = "LOWESS Model 30%")

points(power ~ sample_size, data = sim_sample_size, pch = 20, col = "BLUE")

legend("bottomright", legend = c("smoothed", "original"), lty = c(1, NA), pch = c(NA, 20), col = c("black", "blue"))

##Question 2

plot(
  dat_dispersal$dist.class,
  dat_dispersal$disp.rate.ftb,
  xlim = c(0, 1500),
  xlab = "distance class", 
  ylab = "standardized dispersal rate", 
  main = "Marbled Salamander - NLS exponential fit")

exp_fun =function(x, a, b)
{
  return(a * exp(-b * x))
}

curve(
  exp_fun(x, 6, 0.01), add = TRUE, from = 0, to = 1500,
  ann = FALSE, axes = TRUE, ylab = "f(x)", lty = "dotted"); box()

fit_exp_nls = nls(
  disp.rate.ftb ~ exp_fun(dist.class, a, b),
  data = dat_dispersal,
  start = list(b = 0, a = 1))
summary(fit_exp_nls)


dist_newdata_2 = data.frame(dist.class = seq(0, 1600, length.out = 1600))  

lines(predict(fit_exp_nls, newdata = dist_newdata_2), col= "red")
legend("topright", legend = c("nls fit", "exponential fit"), lty = c("solid", "dotted"), col = c("red", "black"))

##Question 3

dat_all$GCKI_pres = dat_all$GCKI > 0
dat_all$GCKI_pres

fit_gcki_slope = glm(GCKI_pres ~ slope, data = dat_all, family = binomial)
fit_gcki_ba_tot = glm(GCKI_pres ~ ba.tot, data = dat_all, family = binomial)
fit_gcki_both_additive = glm(GCKI_pres ~ slope + ba.tot, data = dat_all, family = binomial)
fit_gcki_both_interactive = glm(GCKI_pres ~ slope * ba.tot, data = dat_all, family = binomial)

AIC(
  fit_gcki_ba_tot,
  fit_gcki_slope,
  fit_gcki_both_additive,
  fit_gcki_both_interactive)

##Question 5

coef_table = coefficients(fit_gcki_both_interactive)

str(coef_table)

fit_1_summary = summary(fit_1)
str(fit_1_summary)

##Question 6

n = 500

slope_newdata = data.frame(
  slope = seq(
    from = min(dat_all$slope, na.rm = T),
    to = max(dat_all$slope, na.rm = T),
    length.out = n
  )
)

ba_newdata = data.frame(
  ba.tot = seq(
    from = min(dat_all$ba.tot, na.rm = T),
    to = max(dat_all$ba.tot, na.rm = T),
    length.out = n))

slope_newdata$gcki_predicted = 
  predict(
    fit_gcki_slope,
    newdata = slope_newdata,
    type = "response"
  )

ba_newdata$gcki_predicted = 
  predict(
    fit_gcki_ba_tot,
    newdata = ba_newdata,
    type = "response"
  )

par(mfrow = c(2, 1))

plot(
  GCKI_pres ~ slope, data = dat_all,
  xlab = "Percent Slope",
  ylab = "GCKI presence/absence",
  pch = 16, cex = 1.5, col = gray(0, 0.2)
)

lines(gcki_predicted ~ slope, data = slope_newdata)

plot(
  GCKI_pres ~ ba.tot, data = dat_all,
  xlab = "Basal Area",
  ylab = "GCKI presence/absence",
  pch = 16, cex = 1.5, col = gray(0, 0.2)
)

lines(gcki_predicted ~ ba.tot, data = ba_newdata)

##Question 7

ba.tot = seq(
  from = min(dat_all$ba.tot, na.rm = T),
  to = max(dat_all$ba.tot, na.rm = T),
  length.out = n)
slope = seq(
  from = min(dat_all$slope, na.rm = T),
  to = max(dat_all$slope, na.rm = T),
  length.out = n)

new_dat_all = expand.grid(
  ba.tot = ba.tot,
  slope = slope)
head(new_dat_all)

tail(new_dat_all)

new_dat_all$pred_add = predict(
  fit_gcki_both_additive,
  newdata = new_dat_all,
  type = "response")

new_dat_all$pred_int = predict(
  fit_gcki_both_interactive,
  newdata = new_dat_all,
  type = "response")

z_gcki_add = matrix(
  new_dat_all$pred_add,
  nrow = length(ba.tot),
  byrow = FALSE)

z_gcki_int = matrix(
  new_dat_all$pred_int,
  nrow = length(ba.tot),
  byrow = FALSE)

par(mfrow = c(1, 2))
contour(
  x = ba.tot, y = slope,
  z = z_gcki_add,
  xlab = "Total Basal Area",
  ylab = "Percent Slope",
  main = "Additive")
contour(
  x = ba.tot,
  y = slope,
  z = z_gcki_int,
  xlab = "Total Basal Area",
  ylab = "Percent Slope",
  main = "Interactive")



