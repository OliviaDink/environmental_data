require(palmerpenguins)
dat_ade = droplevels(subset(penguins, species == "Adelie"))

hist(dat_ade$body_mass_g, main = "Adelie Penguins: Body Mass", xlab = "body mass (g)")

#boxplot 
boxplot(dat_ade$body_mass_g~dat_ade$sex)


#Q2

femalepen=subset(dat_ade,sex== "F" ) 
maleepen=subset(dat_ade,sex== "M" ) 
?t.test

mean(femalepen)

t.test(femalepen$body_mass_g, alternative = c("two.sided", "greater", mu = 0)
       
