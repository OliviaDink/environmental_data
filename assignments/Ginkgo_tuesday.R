dat = read.csv("/Users/oliviadinkelacker/Documents/ECo/environmental_data/data/ginkgo_data_2022.csv")
names(dat)

require(ggplot2)
ggplot(dat, aes(x = max_width, y = notch_depth)) +
  geom_point() +
  xlab("Max Leaf Width (mm)") +
  ylab("Notch Depth (mm)")

ggplot(dat, aes(x = max_width, y = max_depth)) +
  geom_point() +
  xlab("Max Leaf Width (mm)") +
  ylab("Max Leaf Depth (mm)")


ggplot(dat, aes(x = max_width, y = notch_depth, colour = seeds_present)) +
  geom_point() +
  xlab("Max Leaf Width (mm)") +
  ylab("Notch Depth (mm)")

#Q1
unique(subset(dat, seeds_present, site_id))

nrow(dat)

#Q2
uni
dat



#Q3
boxplot()

plot(dat$max_width~dat$max_depth, xlab = "leaf width", ylab ="leaf depth")













