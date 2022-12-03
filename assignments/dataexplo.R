library(here)
dat_habitat <- read.csv(here("data", "hab.sta.csv")) #read data
names(dat_habitat) #check variables

#build histogramm


elevation <- hist(dat_habitat$elev, xlab = "elevation")
aspect <- hist(dat_habitat$aspect, xlab = "aspect")
slope <- hist(dat_habitat$slope, xlab = "slope")

#build scatterplots
s_elevation <- plot(dat_habitat$elev,dat_habitat$ba.tot, main = "Basal area and elevation", xlab = "elevation", ylab = "Total basal area")
s_aspect <- plot(dat_habitat$aspect,dat_habitat$ba.tot, main = "Basal area and aspect", xlab = "aspect", ylab = "Total basal area")
s_slope <- plot(dat_habitat$slope,dat_habitat$ba.tot, main = "Basal area and slope", xlab = "slope", ylab = "Total basal area")

#fit a line
line_point_slope = function(x, x1, y1, slope) #prepare function
{
  get_y_intercept = 
    function(x1, y1, slope) 
      return(-(x1 * slope) + y1)
  
  linear = 
    function(x, yint, slope) 
      return(yint + x * slope)
  
  return(linear(x, get_y_intercept(x1, y1, slope), slope))
}

par(mfrow = c(3, 1))

#creat plot with function for elevation
plot(
  x = dat_habitat$elev, 
  y = dat_habitat$ba.tot,
  xlab = "elevation",
  ylab = "total basal area",
  main = "line for elevation"
)

curve(line_point_slope(x, x1 = 500 , y1 = 20, slope = 0.0), add = TRUE)


#function for aspect

plot(
  x = dat_habitat$aspect, 
  y = dat_habitat$ba.tot,
  xlab = "aspect",
  ylab = "total basal area",
  main = "line for elevation"
)

curve(line_point_slope(x, x1 = 200 , y1 = 20, slope = 0.01), add = TRUE)


#function for slope 

plot(
  x = dat_habitat$slope, 
  y = dat_habitat$ba.tot,
  xlab = "aspect",
  ylab = "total basal area",
  main = "line for elevation"
)

curve(line_point_slope(x, x1 =50 , y1 = 20.0, slope = 0.03), add = TRUE)









