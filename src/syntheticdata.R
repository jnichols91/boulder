rm( list = ls())

library(tidyverse)
library(ggplot2)


load("../data/final-data.rda")
load("../data/boulderMoWater.rda")

#### this set the treatment between Ferric and Alum which Ferric replace with 0 and Alum with 1
x1 <- c(rep(1,72))
x1[full_ancova$coagulant == "Ferric"] <- 0


# mols of metal as covariate 
x2 <- full_ancova %>% select(mols_of_metal_kmol_day,
                             influent_mgd_hourly_avg,
                             effluent_mgd,
                             primary_sludge_gmp_hourly_avg)


#adding noises
y<-  5  - x1 + x2*.1 + .2* rnorm(72)

## data frame 
df <- data.frame( Noise = y, x2, treatment=as.factor( x1))
view(df)

#### plot of the data with two groups

##mols of metal vs noise added mols of metal
df %>% 
  ggplot(aes(mols_of_metal_kmol_day,Noise.mols_of_metal_kmol_day, col = treatment)) +
  geom_point()

##influent_mgd_hourly_avgl vs noise added influent_mgd_hourly_avg
df %>% 
  ggplot(aes(influent_mgd_hourly_avg,Noise.influent_mgd_hourly_avg, col = treatment)) +
  geom_point()

## effluent_mgd vs noise added effluent_mgd
df %>% 
  ggplot(aes(effluent_mgd,Noise.effluent_mgd, col = treatment)) +
  geom_point()

## primary_sludge_gmp_hourly_avg vs noise added primary_sludge_gmp_hourly_avg
df %>% 
  ggplot(aes(primary_sludge_gmp_hourly_avg,Noise.primary_sludge_gmp_hourly_avg, col = treatment)) +
  geom_point()


######### linear regression test of each synthetic data variable
# without covariate mols of metal 
obj0<- lm(Noise.mols_of_metal_kmol_day ~ treatment, data = df)
summary( obj0)

# with covariate mols of metal
obj1<- lm( Noise.mols_of_metal_kmol_day ~ treatment + mols_of_metal_kmol_day , data = df)
summary( obj1)

###########
# without covariate influent_mgd_hourly_avg
obj0<- lm(Noise.influent_mgd_hourly_avg ~ treatment, data = df)
summary( obj0)

# with covariate influent_mgd_hourly_avg
obj1<- lm( Noise.influent_mgd_hourly_avg ~ treatment + influent_mgd_hourly_avg , data = df)
summary( obj1)

############
# without covariate effluent_mgd
obj0<- lm(Noise.effluent_mgd ~ treatment, data = df)
summary( obj0)

# with covariate effluent_mgd
obj1<- lm( Noise.effluent_mgd ~ treatment + effluent_mgd , data = df)
summary( obj1)

###########
# without covariate primary_sludge_gmp_hourly_avg
obj0<- lm(Noise.primary_sludge_gmp_hourly_avg ~ treatment, data = df)
summary( obj0)

# with covariate primary_sludge_gmp_hourly_avg
obj1<- lm( Noise.primary_sludge_gmp_hourly_avg ~ treatment + primary_sludge_gmp_hourly_avg , data = df)
summary( obj1)























