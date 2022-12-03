install.packages("palmerpenguins")
install.packages("here")
require(palmerpenguins)
require(here)

library("palmerpenguins")
class(penguins)

penguins = data.frame(penguins)
class(penguins)

mean(penguins$body_mass_g)
head(penguins$body_mass_g)
head(penguins)


?mean
mean(penguins$body_mass_g, na.rm = TRUE)

summary(penguins)


boxplot(penguins$bill_depth_mm)
plot(penguins$bill_depth_mm)

boxplot(bill_depth_mm ~ sex, data = penguins)


par(mfrow = c(1, 2))
boxplot(penguins$bill_depth_mm)
boxplot(bill_depth_mm ~ sex, data = penguins)


#to creat multiple plots in one space next to each other 
?par


#row equals 1 make it in one row 
coplot(flipper_length_mm ~ bill_length_mm | year, data = penguins, row =1)





































