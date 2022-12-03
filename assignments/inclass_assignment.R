install.packages("here")
require(here)

catrate = read.csv(
  here("data", "catrate.csv")
)

catrate = read.csv(
  here("data", "catrate.csv")
)

delmoys = read.csv(
  here("data", "delomys.csv")
)

rope= read.csv(
  here("data", "rope.csv")
)


head(rope)
head(delmoys)
head(catrate)

class(delmoys)

plot(rope$ch1, rope$ch2, main='Olivia Dinkelacker')

  
  
