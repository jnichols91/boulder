#Packages
library("tidyverse"); theme_set(theme_minimal())
theme_update(panel.grid.minor = element_blank())
library("lubridate")
library("ggplot2")
library("leaps")

#Update r#
install.packages("dataexplore")
library(dataexplore)
plot_missing()

setwd("/Users/arlealcid/Desktop/MOwater")
load(file = "boulderMoWater.rda")
##################################################

#Dosing Daily
##Only Ferric dosing
ferricdose <- subset(dosing_daily, coagulant == "Ferric")

##Only Alum dosing
alumdose <- subset(dosing_daily, coagulant == "Alum")

##No dosing
nodose <- subset(dosing_daily, coagulant == "None")
###################################################

#separate and split phosfax
phosfax.10m <- as_tibble(phosfax_10m) %>% 
  mutate(hour = hour(date)) %>% 
  select(date, hour, everything())

phosfax.hrly <- phosfax.10m %>% 
  group_by(date = date(date), hour) %>% 
  summarise(op_conc_mg_p_l_hourly = mean(op_conc_mg_p_l, na.rm = TRUE)) %>% 
  ungroup()
####################################################

#Find the average op concentration by milligrams per liter 

#ferricaveop is average op (taken from hours) when ferric is added
(ferricaveop <- phosfax.hrly %>%
    filter(date >= ymd("2019-08-15")) %>%
    filter(date <= ymd("2019-09-07")) %>%
    group_by(date) %>%
    summarise(avgop = mean(op_conc_mg_p_l_hourly)))
#join ferricaveop and ferricdose
ferric <- inner_join(ferricaveop, ferricdose, by = c("date"))

#####################################################################
#alumaveop is average op (taken from hours) when alum is added
(alumaveop <- phosfax.hrly %>%
    filter(date >= ymd("2019-10-21")) %>%
    filter(date <= ymd("2019-12-15")) %>%
    group_by(date) %>%
    summarise(avgop = mean(op_conc_mg_p_l_hourly)))
#join alumaveop and alumdose
alum <- inner_join(alumaveop, alumdose, by = c("date"))

#####################################################################
#Compare the Chemicals average OP
par(mfrow=c(1,2))
plot(ferric$mols_of_metal_kmol_day, ferric$avgop, 
     xlab = "Mols of Metals in Kmols Per Day",
     ylab = "Average OP",
     main = "Ferric Dosing")
abline(lm(avgop ~ mols_of_metal_kmol_day, data = ferric), col = "blue")
plot(alum$mols_of_metal_kmol_day, alum$avgop, 
     xlab = "Mols of Metals in Kmols Per Day",
     ylab = "Average OP",
     main = "Alum Dosing")
abline(lm(avgop ~ mols_of_metal_kmol_day, data = alum), col = "blue")
#####################################################################
#Compare Return Activated Sludge
#Ferric
ferricsludge <- flow.hrly %>%
  filter(date >= ymd("2019-08-16")) %>%
  filter(date <= ymd("2019-09-07")) 
plot(ferricsludge$date, ferricsludge$ras_gpm, 
     xlab = "Date",
     ylab = "Return Activated Sludge by Gallons Per Minute",
     main = "Ferric Sludge")
mean(ferricsludge$ras_gpm)

#Alum
alumsludge <- flow.hrly %>%
  filter(date >= ymd("2019-10-21")) %>%
  filter(date <= ymd("2019-12-15"))
plot(alumsludge$date, alumsludge$ras_gpm,
     xlab = "Date",
     ylab = "Return Activated Sludge by Gallons Per Minute",
     main = "Alum Sludge")
mean(alumsludge$ras_gpm)
#####################################################################
