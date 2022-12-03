# Clear your R environment to make 
# sure there are no stray variables.

rm(list = ls())

pol_n_predation = 26
pol_n_no_predation = 184
pol_n_total = 210
  pol_predation_rate = pol_n_predation/pol_n_total
  
  psd_n_predation = 25
  psd_n_no_predation = 706
  psd_n_total = 732
  psd_predation_rate = psd_n_predation/psd_n_total #calculate proportion rate
  

#write sentences with values 
  
  print(
    paste0(
      "The seed predation rate for Polyscias fulva is: ",
      round(pol_predation_rate, digits = 3))) 

print(
  paste0(
    "The seed predation rate for Pseudospondias microcarpa is: ",
    round(psd_predation_rate, digits = 3)))



#calculate ratio of seed predatipn proportions 
pol_predation_rate/psd_predation_rate




