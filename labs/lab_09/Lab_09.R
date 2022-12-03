library(here)
catrate = read.csv(here("data", "catrate.csv"))
head(catrate)


#binom test #how likely 
n_success = sum(catrate$success)
n_years = sum(catrate$years)
binom.test(
  x = n_success,
  n = n_years,
  p = 0.5)

#salamander testing repdoructive rate 
late_fill_rate = 2/7
normal_fill_rate = 1 - late_fill_rate
binom.test(
  x = n_success,
  n = n_years,
  p = normal_fill_rate) 

#alternative hypothesis
binom.test(
  x = n_success,
  n = n_years,
  p = normal_fill_rate,
  alternative ='less')

#t.test comparison
t.test(catrate$cat.rate, mu = 2/7)


#does sample variance differ #if variances the same around one
veg = read.csv(here("data", "vegdata.csv"))
veg = read.csv("/Users/oliviadinkelacker/Documents/ECo/environmental_data/data/vegdata.csv")
head(veg)

boxplot(pine ~ treatment, data = veg)

#variance test between two treatments
veg2 = droplevels(
  subset(
    veg,
    treatment %in% c('control','clipped')
  ))

# verify that treatment is factorized
veg2$treatment = factor(veg2$treatment)

var.test(
  pine ~ treatment,
  data = veg2)

#F tests for unequal variances assumes normnality/parametric
#test for nornlaity 
shapiro.test(veg2$pine[veg2$treatment=="control"])
shapiro.test(veg2$pine[veg2$treatment=="clipped"])

#Non-parametric Variance Test
fligner.test(
  pine ~ treatment,
  data = veg2)

#Tests for multiple variances #all four treatments 
#parametric 
bartlett.test(pine ~ treatment, data = veg)


#non-parametric 
fligner.test(pine ~ treatment, data = veg)


#Comparing two sample means
#t.test #asusmptions
t.test(
  pine ~ treatment,
  data = veg2)


#non-parametric errors 
wilcox.test(
  pine ~ treatment,
  data = veg2)


#paired samples #expect correlation 
install.packages("datarium")
require(datarium)
data("mice2")
head(mice2)

t.test(mice2$before, mice2$after, paired = TRUE)

#paired test
wilcox.test(
  mice2$after ~ mice2$before,
  data= mice2)

#unpaired test
t.test(mice2$before, mice2$after, paired = FALSE)

####salamadners
disp = read.csv(here("data", "dispersal.csv"))
disp= read.csv("/Users/oliviadinkelacker/Documents/ECo/environmental_data/data/dispersal.csv")
plot(
  disp.rate.ftb ~ disp.rate.eb,
  data = disp,
  main = "Marbled Salamander Dispersal Rates",
  xlab = "Dispersal Rate\nFirst Time Breeders",
  ylab = "Dispersal Rate\nExperienced Breeders",
  pch = 21, col = 1, bg = "steelblue"
)

# test correlation  #parametric 
cor.test(
  disp$disp.rate.ftb,
  disp$disp.rate.eb,
  use='complete.obs')

#non-parametric
cor.test(
  disp$disp.rate.ftb,
  disp$disp.rate.eb,
  use='complete.obs',
  method='spearman')

##comparing two distributions
#compare the empirical cumulative distributions
#give the probability that a randomly selected value of X is less than or equal to x
plot(
  ecdf(disp$disp.rate.ftb),
  verticals=TRUE,
  main = "Mike's Plot of Marbled Salamanders\nFirst-Time Breeders: ECDF")

#add adult dispersial rate
plot(
  ecdf(disp$disp.rate.ftb),
  verticals=TRUE,
  main = "Mike's Plot of Marbled Salamanders\nFirst-Time and Experienced Breeders: ECDF")
plot(
  ecdf(disp$disp.rate.eb),
  verticals=TRUE,
  lty=3,
  add=TRUE)
legend(
  x = 0.4, y = 0.4,
  lty = c(1, 3),
  legend = c("first-time", "experienced"),
  title = "Breeder Class")


#are distributions different
ks.test(disp$disp.rate.ftb,disp$disp.rate.eb)

#Comparing two or more proportions
#sex-linked killing
prop.test(
  x = c(4,16),
  n = c(40,250))

#contigency: chi-squared test
#significance of the differences between observed and expected frequencies
owls = matrix(c(16, 9, 4, 11), nrow=2)
rownames(owls) = c("present", "absent")
colnames(owls) = c("old", "young")
chisq_owls = chisq.test(owls)
chisq_owls

chisq.test(chisq_owls)

#Chi-Square Expected and Observed Values
#significant p-value doesn’t tell us whether barred owls are present more or less frequently than expected.

#the expcected value 
round(chisq_owls$expected, 1)

#the observed value
chisq_owls$observed

#Chi-Sqare Residuals
round(
  chisq_owls$observed - chisq_owls$expected,
  digits = 1)

#Fisher’s Exact test #when values smaller than 4
fisher.test(owls)

#Bird habitat data
birds   = read.csv(here("data", "bird.sta.csv"))
birds= read.csv("/Users/oliviadinkelacker/Documents/ECo/environmental_data/data/bird.sta.csv")
birds
hab     = read.csv(here("data", "hab.sta.csv"))
hab = read.csv("/Users/oliviadinkelacker/Documents/ECo/environmental_data/data/hab.sta.csv")
birdhab = merge(
  birds,
  hab, by=c("basin", "sub", "sta"))

# Create a contingency table for edge/interior and brown creeper presence/absence
table(
  birdhab$s.edge,
  birdhab$BRCR > 0)


# set the presence to be in the first column
br_creeper_table = table(
  birdhab$s.edge, 
  birdhab$BRCR > 0)[, 2:1]

br_creeper_table

chisq.test(br_creeper_table)

#Q1
chisq.test(br_creeper_table)

#Q3
install.packages("palmerpenguins")
require(palmerpenguins)
fit_fl_sp = 
  lm(
    formula = flipper_length_mm ~ species,
    data = penguins)


require(palmerpenguins)

fit_species = 
  lm(
    formula = penguins$body_mass_g ~ penguins$species,
    data = penguins)

fit_species


fit_sex =  lm(
  formula = penguins$body_mass_g ~ penguins$sex,
  data = penguins)

fit_both =  lm(
  formula = penguins$body_mass_g ~ species:sex,
  data = penguins)

fit_both

#Q6

boxplot(penguins$body_mass_g ~ penguins$sex:penguins$species,
        main = "Doubly conditional boxplot",
        las = "2",
        xlab = " ",
        ylab = "Body mass",
        names = c("Femal\nAdelie", "Male\nAdelie", "Femal\nChinstrap", "Malen\nChinstrap", "Female\nGentoo", "Male\nGentoo"))



boxplot(penguins$body_mass_g ~ penguins$species,
        main = "boxplot fit_sex",
        xlab = "species",
        ylab = "body mass")


boxplot(penguins$body_mass_g ~ penguins$sex,
        main = "boxplot fit_sex",
        xlab = "sex",
        ylab = "body mass")

#Q10
bartlett.test(penguins$body_mass_g ~ penguins$species)

#Q11
bartlett.test(penguins$body_mass_g ~ penguins$sex)



#12
#Q11
bartlett.test(penguins$body_mass_g ~ penguins$sex:penguins$species)

#Q13
dat_groups = aggregate(
  body_mass_g ~ sex:species,
  data = penguins,
  FUN = c)
str(dat_groups)


dat_groups$body_mass_g

bartlett.test(dat_groups$body_mass_g)

#Q15
trees_FL.csv

library(here)
dat_fl = read.csv("/Users/oliviadinkelacker/Documents/ECo/environmental_data/data/trees_FL.csv")

?barplot

names(dat_fl)

barplot(dat_fl$ProbabilityofFailure)


barplot()

hist(dat_fl$DBH_in, main = "histogram of DBH", xlab = "DBH")
plot(dat_fl$HeighttoTop_ft~dat_fl$DBH_in, main = "DBH and tree height", xlab = "DBH", ylab ="Height")

barplot(table(dat_fl$ProbabilityofFailure), ylab = "count of trees", xlab = "probability of failure class", main = " failure classes")

barplot(table(dat_fl$Failure_Standardized), xlab = "failure class", ylab = "count of trees", main = " probability of failure classes")

par(mfrow = c(2,2))

#Q16

Are two sample distributions the same, or are they significantly different from one another in one or more (unspecified) ways?

  
 ks.test(disp$disp.rate.ftb,disp$disp.rate.eb)
ks.test(dat_fl$DBH_in
          .rate.ftb,disp$disp.rate.eb)


whole = dat_fl(droplevels $DBH_in =="whole")

whole = droplevels(subset(dat_fl, Failure_Standardized == "whole"))

none = droplevels(subset(dat_fl, Failure_Standardized == "none"))
branch = droplevels(subset(dat_fl, Failure_Standardized == "whole"))


?ks.test

ks.test(f, data, subset, na.action, ...)

ks.test(none, dat_fl$DBH_in)

ks.test(whole$DBH_in, none$DBH_in)

plot()



cor.test(
  dat_fl$DBH_in,
  dat_fl$HeighttoTop_ft)

cor.test(dat_fl$DBH_in, dat_fl$HeighttoTop_ft)




resids <- residuals(fit_species)
##20-25

dat_fl$fail = factor(dat_fl$Failure_Standardized != "none")

levels(dat_fl$fail) = c("No Fail", "Fail")


fl_table_2 = table(
  dat_fl$ProbabilityofFailure,
  dat_fl$fail)
fl_table_2

chisq_fl = chisq.test(fl_table_2)

chisq_fl$o
#residuals
chisq_fl$expected
chisq_fl$observed
round(
  chisq_fl$observed - chisq_fl$expected,
  digits = 1)














