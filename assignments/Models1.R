dat_ade = droplevels(subset(penguins, species == "Adelie"))

femalepen=subset(dat_ade,sex== "female" )  
t.test(female_pen$body_mass_g, alternative = "greater", mu = 0, paired = FALSE, var.equal = FALSE) 


malepen <- subset(dat_ade,sex== "male" ) 



t.test(malepen$body_mass_g, alternative = "greater", mu = 4000, paired = FALSE, var.equal = FALSE)



t.test(femalepen$body_mass_g, malepen$body_mass_g, alternative = "greater")

t.test(dat_ade$body_mass_g  dat_ade$sex, alternative = "greater",  paired = FALSE, var.equal = FALSE))



t.test(malepen$body_mass_g, femalepen$body_mass_g, alternative = "greater", paired = TRUE)



t.test(malepen$body_mass_g, femalepen$body_mass_g, alternative = "less", paired = TRUE, var.equal = FALSE) 

greater = is the alternative that x has a larger mean than y 
> put y or x at first 



