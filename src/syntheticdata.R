rm( list = ls() )

library(tidyverse)
library(fields)
library(lubridate)
library(latex2exp)
library(rstatix)
library(emmeans)
library(ggpubr)

load("../data/final-data.rda")
load("../data/boulderMoWater.rda")


x1 <- c(rep(1,72))
x1[1:32] <-0
x1

# mols of metal as covariate 
x2 <- full_ancova %>% select(mols_of_metal_kmol_day,
                             influent_mgd_hourly_avg,
                             effluent_mgd,
                             primary_sludge_gmp_hourly_avg)


#adding noises
y<-  5  - x1 + x2*.1 + .2* rnorm(72)
y


df<- data.frame( Y=y, X = x2, treatment=as.factor( x1))
view(df)



df %>% 
  ggplot(aes(mols_of_metal_kmol_day,mols_of_metal_kmol_day.1, col = treatment)) +
  geom_point()

obj0<- lm( mols_of_metal_kmol_day.1 ~ treatment, data = df)
summary( obj0)

# with covariate shows  group one is better
obj1<- lm( mols_of_metal_kmol_day.1 ~ treatment + mols_of_metal_kmol_day , data = df)
summary( obj1)

