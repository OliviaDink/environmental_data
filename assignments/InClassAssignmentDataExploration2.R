dat_birds = read.csv("https://michaelfrancenelson.github.io/environmental_data/data/bird.sta.csv") #import data
dat_habitat = read.csv("https://michaelfrancenelson.github.io/environmental_data/data/hab.sta.csv")


pairs(dat_habitat[, c("lat", "long", "slope")]) #create pair plot
class(dat_habitat)
class(dat_birds)
names(dat_birds)
hist(dat_birds$OCWA, xlab = "Number of birds counted", breaks = 0:5 - 0.5)
