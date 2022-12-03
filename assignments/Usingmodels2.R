##Using models 2

#1-sample t-test
library(palmerpenguins)
t.test(subset(penguins, species == "Gentoo")$flipper_length_mm)


#test if legnth is equal to 218
t.test(
  x = subset(penguins, species == "Gentoo")$flipper_length_mm,
  mu = 218
)

#test if length is smaller thank 218
t.test(
  x = subset(penguins, species == "Gentoo")$flipper_length_mm,
  mu = 218,
  alternative = "less"
)


#2-sample t-test
t.test(flipper_length_mm ~ species, data = subset(penguins, species != "Chinstrap"))

#1 - Analysis of Variance (ANOVA)
#Data exploration
#Graphical
par(mfrow = c(1, 2))
hist(penguins$body_mass_g, breaks = 80, main = "histogram of body mass", xlab = "body mass (g)")
plot(density(penguins$body_mass_g, na.rm = TRUE), main = "density plot of body mass")

require(palmerpenguins)
boxplot(body_mass_g ~ species, data = penguins)

#numerical
dat_chinstrap = subset(penguins, species == "Chinstrap")
mean(dat_chinstrap$body_mass_g, na.rm = TRUE)
shapiro.test(dat_chinstrap$body_mass_g)


#shortcut for calculating the species mean body masses
aggregate(body_mass_g ~ species, data = penguins, FUN = mean)

aggregate(
  body_mass_g ~ species,
  data = penguins,
  FUN = function(x) shapiro.test(x)$p.value)


#Fit a linear model
fit_species = lm(body_mass_g ~ species, data = penguins)

#look at the model coefficients 
summary(fit_species)

#Conduct the ANOVA
anova(fit_species)

#One-Way Anova Complete Walkthrough
fit_species = lm(body_mass_g ~ species, data = penguins)
summary(fit_species)
anova(fit_species)


#Two-Way Additive ANOVA
boxplot(body_mass_g ~ species, data = penguins)

#Fit a 2-way, additive model.
fit_additive = lm(body_mass_g ~ sex + species, data = penguins)

#Two-Way interactive (factorial) ANOVA
fit_interactive = lm(body_mass_g ~ sex * species, data = penguins)

summary(fit_interactive)


#Simple Linear Regression: Penguin Bills and Body Mass
lm(bill_length_mm ~ body_mass_g, data = penguins)

#Q1
boxplot(penguins$body_mass_g ~ penguins$sex:penguins$species,
        main = "Conditional boxplot with 2 predictors",
        las = "2",
        xlab = " ",
        ylab = "Body mass",
        names = c("Femal\nAdelie", "Male\nAdelie", "Femal\nChinstrap", "Malen\nChinstrap", "Female\nGentoo", "Male\nGentoo"))



#Q4
fit_both = lm(body_mass_g ~ sex * species, data = penguins)
summary(fit_both) #see coefficient table 

3368.84+158.37



#Q6
aggregate(body_mass_g ~ species:sex, data = penguins, FUN = mean)

dat_chinstrap2 = subset(penguins, species == "Chinstrap")

dat_chinstrap2 = subset(dat_chinstrap2, sex == "female")
dat_chinstrap2


