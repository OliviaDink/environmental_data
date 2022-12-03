data(iris)
fit_species = 
  lm(
    Sepal.Length ~ Species,
    data = iris)

summary(fit_species)

boxplot(
  Petal.Length ~ Species,
  data = iris,
  xlab = "Petal Length (cm)",
  ylab = "Petal Width (cm)")


summary(fit_petals)


5.0060  + 1.5820

iris

mean(iris$Sepal.Length == setosa)

boxplot(resids)
iris



?shapiro.test()


shapiro.test(resids)


resids <- residuals(fit_species)


summary()


fit_petals = 
  lm(
    Petal.Width ~ Petal.Length,
    data = iris)

summary(fit_length)


-0.363076 * 4


