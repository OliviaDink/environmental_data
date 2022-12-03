#load data
catrate = read.csv("/Users/oliviadinkelacker/Documents/ECo/environmental_data/data/catrate.csv")
head(catrate)

summary(catrate)

#histogramm of catastrophic rates
hist(catrate$cat.rate, main = "Histogramm of Catastrophic Rates", xlab = "Catastrophic Rate")


#check for normality shapiro
shapiro.test(catrate$cat.rate)

# t.test
?t.test
t.test(catrate$cat.rate, mu = 2/7)

#one sided alternative hypothesis
t.test(catrate$cat.rate, mu = 2/7, alternative = "greater")

#non parametric 
wilcox.test(catrate$cat.rate, mu = 2/7)

#comparing two sample means
penguin_dat = droplevels(subset(penguins, species != "Gentoo"))
require(palmerpenguins)
summary(penguin_dat)

boxplot(
  flipper_length_mm ~ species, 
  data = penguin_dat,
  ylab = "Flipper Length (mm)")


#test for normality
# Extract the Adelie penguin data
dat_adelie = subset(penguin_dat, species == "Adelie")
dat_chin = subset(penguin_dat, species == "Chinstrap")


shapiro.test(dat_adelie$flipper_length_mm)
shapiro.test(dat_chin$flipper_length_mm)

#Parametric and Nonparametric Tests
t.test(penguin_dat$flipper_length_mm ~ penguin_dat$species)


levels(penguin_dat$species)



#Q6
t.test(catrate$cat.rate, mu = 2/7, alternative = "two.sided")


par(mfrow= c(1,2))

hist(dat_adelie$flipper_length_mm, main = "Adelie flipper length", xlab = "flipper length")
hist(dat_chin$flipper_length_mm, main = "Chinstrap flipper length", xlab = "flipper length")

t.test(dat_adelie$flipper_length_mm, dat_chin$flipper_length_mm)













































