library(here)
habitat <- read.csv(here("data", "hab.sub.csv"))
birds <- read.csv(here("data", "bird.sub.csv"))
veg  <- read.csv(here("data", "vegdata.csv"))

#drop levels
library(palmerpenguins)
dat_pen = droplevels(subset(penguins, species != "Gentoo"))
boxplot(dat_pen$flipper_length_mm ~ dat_pen$species)

#parametric t.tets
t.test(flipper_length_mm ~ species, data = dat_pen, alternative = "two.sided")






#two.boot
install.packages("simpleboot")
library(simpleboot)
?two.boot

adelie <- droplevels(subset(dat_pen, species != "Chinstrap", na.rm = TRUE))
chinstrap <- droplevels(subset(dat_pen, species != "Adelie", na.rm= TRUE))

adelie <- adelie$flipper_length_mm
chinstrap <- chinstrap$flipper_length_mm

pen_boot <- two.boot(adelie, chinstrap, na.rm =TRUE, FUN= mean, R = 1000)
str(pen_boot)

boot.ci(pen_boot)
quantile(pen_boot$t, c(0.025, 0.975))

mean(pen_boot$t)
median(pen_boot$t)


pen_boot

hist(pen_boot$t, xlab  = "difference in means", main = "flipper length differences Adelie and Chinstrap")
sd(pen_boot$t)

#tree data 
boxplot(pine ~ treatment, dat = veg)
#select control and clipped
dat_tree = droplevels(subset(veg, treatment %in% c("control", "clipped")))
boxplot(dat_tree$pine ~ dat_tree$treatment)
table(dat_tree$pine)

#non-parametric t-test
?wilcox.test()
wilcox.test(pine~treatment, data = dat_tree, alternative = "two.sided")


#Bootstrap CI

require(boot)
clipped <- droplevels(subset(dat_tree, treatment == "clipped", na.rm = TRUE))
control <- droplevels(subset(dat_tree, treatment == "control", na.rm = TRUE))

clipped <- clipped$pine
control <- control$pine

tree_boot <- two.boot(clipped, control, FUN= mean, R = 1000, na.rm =TRUE)
str(tree_boot)
boot.ci(tree_boot, na.rm=TRUE)
quantile(tree_boot$t, c(0.025, 0.95))

mean(tree_boot$t)


##Bird data
# I already read my data into dat_bird and dat_habitat:
dat_all = merge(
  birds, 
  habitat,
  by = c("basin", "sub"))

head(dat_all[, c("b.sidi", "s.sidi")])

# Calculate the sample mean and sd:
b_sidi_mean = mean(dat_all$b.sidi, na.rm = TRUE)
b_sidi_sd   = sd(dat_all$b.sidi, na.rm = TRUE)

# Use the subset-by-name symbol ($) to create a 
# new column of z-standardized values.

dat_all$b.sidi.standardized = (dat_all$b.sidi - b_sidi_mean)/b_sidi_sd

mean(dat_all$b.sidi.standardized)
sd(dat_all$b.sidi.standardized)

##same for s.sidi
# Calculate the sample mean and sd:
s_sidi_mean = mean(dat_all$s.sidi, na.rm = TRUE)
s_sidi_sd   = sd(dat_all$s.sidi, na.rm = TRUE)

# Use the subset-by-name symbol ($) to create a 
# new column of z-standardized values.

dat_all$s.sidi.standardized = (dat_all$s.sidi - s_sidi_mean)/s_sidi_sd

mean(dat_all$s.sidi.standardized)
sd(dat_all$s.sidi.standardized)


##model variables ## plotting simpson
plot(
  b.sidi ~ s.sidi, data = dat_all,
  main = "Simpson's diversity indices",
  xlab = "Vegetation cover diversity",
  ylab = "Bird diversity")


#simple linear regression 
fit_1 = lm(b.sidi ~ s.sidi, data = dat_all)
coef(fit_1)

##add line to plot
slope_observed = coef(fit_1)[2]
plot(
  b.sidi ~ s.sidi, data = dat_all,
  main = "Simpson's diversity indices",
  xlab = "Vegetation cover diversity",
  ylab = "Bird diversity")
abline(fit_1)


##the slope coefficient
dat_1 = 
  subset(
    dat_all,
    select = c(b.sidi, s.sidi))

##resample with Monte Carlo
set.seed(123)
index_1 = sample(nrow(dat_1), replace = TRUE)
index_2 = sample(nrow(dat_1), replace = TRUE)

dat_resampled_i = 
  data.frame(
    b.sidi = dat_1$b.sidi[index_1],
    s.sidi = dat_1$s.sidi[index_2]
  )

fit_resampled_i = lm(b.sidi ~ s.sidi, data = dat_resampled_i)
slope_resampled_i = coef(fit_resampled_i)[2]

print(slope_resampled_i)

##scatterplot 
plot(
  b.sidi ~ s.sidi, data = dat_resampled_i,
  main = "Simpson's diversity indices (MC resampled data)",
  xlab = "Vegetation cover diversity",
  ylab = "Bird diversity")
abline(fit_resampled_i)

##Monet Carlo LOOP
m = 10000 
result_mc = numeric(m) 

for(i in 1:m)
{
  index_1 = sample(nrow(dat_1), replace = TRUE)
  index_2 = sample(nrow(dat_1),  replace = TRUE)
  
  dat_resampled_i = 
    data.frame(
      b.sidi = dat_1$b.sidi[index_1],
      s.sidi = dat_1$s.sidi[index_2]
  )

  fit_resampled_i = lm(b.sidi ~ s.sidi, data = dat_resampled_i)
  slope_resampled_i = coef(fit_resampled_i)[2]
  
  result_mc[i] = coef(fit_resampled_i)[2]
} 

hist(result_mc, main = "histogram of MC results", xlab = "results")




##null distribution 
hist(
  result_mc,
  main = "Mike's Null Distribution of Regression Slope",
  xlab = "Slope Parameter")
abline(v = slope_observed, lty = 2, col = "red", lwd = 2)
abline(v=q, lty = 2, col = "blue", lwd = 2) 
legend("topright",
       legend= c("observed value", "critical value"), 
lty= c(2,2), col = c("red", "blue"), inset = c(0.03, 0.03))


##critical Slope Values 
q = quantile(result_mc, c(.05))
slope_observed = coef(fit_1)[2]
print(slope_observed)
##alternative distribution bootstrapping

set.seed(345)
index_1 = sample(nrow(dat_1), replace = TRUE)

dat_boot = dat_1[index_1, ]
head(dat_boot)

###build linear model
fit_bs1 = lm(b.sidi ~ s.sidi, data = dat_boot)

coef(fit_bs1)



##Monet Carlo LOOP
m = 10000 
result_boot = numeric(m) 

for(i in 1:m)
{
  index_1 = sample(nrow(dat_1), replace = TRUE)
  
  dat_boot = dat_1[index_1, ]
  head(dat_boot)
  
  fit_bs1 = lm(b.sidi ~ s.sidi, data = dat_boot)
  coef(fit_bs1)
  
  result_boot[i] = coef(fit_bs1)[2]
} 

###LOOP 
hist(
  result_boot,
  main = "Mike's Alternative Distribution of Regression Slope",
  xlab = "Slope Parameter")
abline(v = slope_observed, lty = 2, col = "red", lwd = 2)
abline(v = 0, lty = 2, col = 1, lwd = 2)


##compare null and alternative 
par(mfrow = c(1, 2))
hist(result_boot, xlab = "slope")
hist(result_mc, xlab = "slope")

##density plot of null distribution
plot(
  density(result_mc),
  main = "Null and alternative Distribution",
  xlab = "Slope Coefficient", ylim = c(0,65), xlim = c(-0.05, 0.04), col = "red")
lines(density(result_boot), col = "blue")
legend("topright",
       legend= c("null", "alternative"), lty= c(1,1),
      col = c("red", "blue"), inset = c(0.03, 0.03))

##density plot of null distribution
plot(lines(
  density(result_boot))



##lines function 
?lines

lines(
  density(result_boot), (results_mc),
  main = "Mike's Null Distribution Density Plot",
  xlab = "Slope Coefficient",  add = TRUE)



#Q5

##density plot of null distribution

plot(
  density(result_mc),
  main = "Mike's Null Distribution Density Plot",
  xlab = "Slope Coefficient", ylim = c(0,65), xlim = c(-0.05, 0.04))
lines(density(result_boot))


pen_ecdf = ecdf(pen_boot$t)
1-pen_ecdf(-4.5)


#8
pen_ecdf(-8)












