rm(list = ls())

rope = read.csv("/Users/oliviadinkelacker/Documents/ECo/environmental_data/data/rope.csv")
rope$rope.type = factor(rope$rope.type)
levels(rope$rope.type)
                        
n_obs = nrow(rope)
n_obs
n_groups = length(unique(rope$rope.type))
n_groups

                        
ss_tot = sum(resids^2)
ss_tot
grandmean = mean(rope$p.cut)
obs = rope$p.cut
resids = obs - grandmean
df_tot = n_obs-1


agg_mean = aggregate(
  x = rope$p.cut,
  by = list(rope$rope.type), 
  FUN = mean)

aggregate(
  x = rope$p.cut,
  by = list(rope$rope.type),
  FUN = function(x) mean(x))

agg_resids = aggregate(
  x = rope$p.cut,
  by = list(rope$rope.type),
  FUN = function(y) y-mean(y))


str(agg_resids)

agg_sum_sq_resids = aggregate(
  x = rope$p.cut,
  by = list(rope$rope.type),
  FUN = function(y) sum((y-mean(y))^2))

str(agg_sum_sq_resids)

ss_within = sum(agg_sum_sq_resids$x)
ss_within
df_within = n_obs-n_groups
df_within

#Partitioning Variance: Among Groups
ss_among = ss_tot - ss_within
ss_among
df_among = n_groups-1
df_among

ms_among  =  ss_among / (n_groups - 1)
ms_within = ss_within / (n_obs - n_groups)
ms_among
ms_within

#The Test Statistic: F
f_ratio = ms_among /ms_within
f_ratio

f_pval = 1-pf(f_ratio,df_among,df_within)
f_pval

#ANOVA in R

fit_1 = lm(p.cut ~ rope.type, data=rope)
anova(fit_1)

anova_fit_1 = anova(fit_1)
str(anova_fit_1)

anova_fit_1$Sum Sq
anova_fit_1$"Sum Sq"


#Post-Hoc Testing #pairwise comparisons between each possible pair of groups.

rope2 = droplevels(
  subset(
    rope,
    rope.type %in% c("XTC")))
    
mean(rope2$p.cut)

boxplot(
  p.cut ~ rope.type,
  data = rope2,
  las = 2,
  xlab = "",
  ylab = "Proportion Rope Cut",
  main = "Subset of Rope Data")
mtext("Rope Type", side = 1, line = 3)

#testing
fit_rope_2 = lm(p.cut ~ rope.type, data=rope2)
rope2_hsd = TukeyHSD(aov(fit_rope_2))
class(rope2_hsd)

round(rope2_hsd$rope.type, digits = 4)


#Q3
bartlett.test(p.cut~rope.type, data = rope)

bartlett.test(body_mass_g~species, data = penguins)



#5
fit_rope_1 = lm(p.cut ~ rope.type, data = rope)
summary(fit_rope_1)

mean(rope$rope.type == "BLAZE")


#8
shapiro.test(residuals(fit_rope_1))

#retrieve p value for every group
lapply(agg_resids$x,
       function(x) shapiro.test(x)$p)


#12
require(palmerpenguins)
pen_fem = subset(penguins, sex == "female")
boxplot(pen_fem$body_mass_g ~ pen_fem$species, xlab = "Species", ylab= "Body mass")

bartlett.test(body_mass_g~species, data = pen_fem)


pen = lm(body_mass_g ~ species, data = pen_fem)

shapiro.test(residuals(pen))

TukeyHSD(aov(pen))









