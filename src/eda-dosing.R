library(fields)
library(dplyr)
library(tidyverse)
library(lubridate)
library(tibble)


load("/Users/haosheng/Desktop/MoWater_Summer_Intern/boulderMoWater.rda")

ferric_none<-as_tibble(dosing_daily) %>% 
   filter(date(date) >= ymd("2019-08-15")) %>% 
            filter(date(date) <= ymd("2019-10-20"))
 
alum_none<- as_tibble(dosing_daily) %>% 
   filter(date(date) >= ymd("2019-10-21")) %>% 
   filter(date(date) <= ymd("2020-1-31"))

alum <- dosing_daily %>% 
  filter(coagulant == "Alum") 
alum<-alum[-142,]



ferric <- dosing_daily %>% 
  filter(coagulant == "Ferric")
view(ferric)
view(alum)


#####flow gal day
plot(ferric_none$date,ferric_none$flow_gal_day)
plot(alum_none$date,alum_none$flow_gal_day)

boxplot(ferric_none$flow_gal_day)
####flow_avg_ppmv
plot(ferric_none$date,ferric_none$flow_avg_ppmv)
plot(alum_none$date,alum_none$flow_avg_ppmv)
#### outliers for solution mass ibs day in alum
boxplot(alum$solution_mass_lbs_day)
plot(alum$date,alum$solution_mass_lbs_day)
#### solution mass ibs day in ferric
boxplot(ferric_none$solution_mass_lbs_day)
plot(ferric_none$solution_mass_lbs_day)
###salt mass kg day ferric
plot(ferric_none$date,ferric_none$salt_mass_kg_day)
boxplot(ferric_none$salt_mass_kg_day)
### salt mass kf day alum + outliers
plot(alum_none$date,alum_none$salt_mass_kg_day)
boxplot(alum$salt_mass_kg_day)

test<- alum%>% filter(salt_mass_kg_day<1100)
  
boxplot(test$salt_mass_kg_day)
#### mols of metal ferric
boxplot(ferric_none$mols_of_metal_kmol_day)
plot(ferric_none$mols_of_metal_kmol_day)
#### mols of metal alum
boxplot(alum$mols_of_metal_kmol_day)
plot(alum$date,alum$mols_of_metal_kmol_day)



