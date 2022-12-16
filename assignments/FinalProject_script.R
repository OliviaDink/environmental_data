#######Part2: Data Analysis 

#1 Data exploration 
#1.1 Numerical exploration
require(here)
delomys <- read.csv(here("data", "delomys.csv"))
View(delomys)

#use summary 
summary(delomys$body_mass)
summary(delomys$body_length)

#normality test
shapiro.test(delomys$body_mass)
shapiro.test(delomys$body_length)
#not normal 

#1.2 Graphical exploration 
plot(delomys$body_length ~ delomys$body_mass)
par(mfrow= c(1,2))
hist(delomys$body_mass, breaks = 50)
hist(delomys$body_length, breaks =50)

par(mfrow= c(2,2))
boxplot(delomys$body_mass~delomys$binomial)
boxplot(delomys$body_mass~delomys$sex)
boxplot(delomys$body_mass~delomys$sex:delomys$binomial)


#2.1 Model building 
#linear regression 
fit1 <- lm(delomys$body_length ~ delomys$body_mass)

#1-way ANOVA with two levels 
fit2 <- lm(delomys$body_mass ~ delomys$sex)

#1-way ANOVA with two levels #species
fit3 <- lm(delomys$body_mass ~ delomys$binomial)

#2-way additive ANOVA 
fit4 <- lm(delomys$body_mass ~ delomys$sex + delomys$binomial)

#2-way factorial ANOVA
fit5 <- lm(delomys$body_mass ~ delomys$sex * delomys$binomial)



#2.2 Model Diagnostics
fit1 <- residuals(fit1)
fit2 <- residuals(fit2)
fit3 <- residuals(fit3)
fit4 <- residuals(fit4)
fit5 <- residuals(fit5)

hist(fit1)

shapiro.test(fit1)
shapiro.test(fit2)
shapiro.test(fit3)
shapiro.test(fit4)
shapiro.test(fit5)


#Model interpretation 
#Print model coefficient table
knitr::kable(coef(summary(fit1)))
76.1246565 + (0.8754988*0)
76.1246565 + (0.8754988*100)

knitr::kable(coef(summary(fit2)))

#Print ANOVA table 
?dnorm
knitr::kable(anova(fit5))

anova(fit2)
summary(fit1)

magnitude 
0.551

 18 
 has to be 
has the lowest 18

summar

#find best model using the AIC #lowest AIC value indicates the best model fit 

AIC(fit2)
AIC(fit3)
AIC(fit4)
AIC(fit5)

d value 





r squared closer to 1 closer to 

r squared is magnitude 

presentation upload 


