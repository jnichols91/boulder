rm( list = ls() )

library(tidyverse)
library(fields)
library(lubridate)
library(latex2exp)
library(reshape2)


# make sure src is the current working directory

load("../data/final-data.rda")
load("../data/boulderMoWater.rda")

# display the number of observations for each type of coagulant

table(final_data$coagulant)

# splitting the data by coagulant 

ferr_data <- final_data %>% filter(coagulant == "Ferric")
alum_data <- final_data %>% filter(coagulant == "Alum")
none_data <- final_data %>% filter(coagulant == "None")

# average was found by hour maybe do a moving average instead?  

ts.plot(alum_data$op_conc_mg_p_l_hourly)
plot(alum_data$date, alum_data$op_conc_mg_p_l_hourly)

# gap in november looking at this variable at original time scale no average

alumphos <- phosfax_10m %>%
  filter(date >= ymd("2019-10-22")) %>%
  filter(date <= ymd("2019-12-15"))
plot(alumphos$date, alumphos$op_conc_mg_p_l,
     cex = .25,
     main = "Alum effluent OP",
     ylim = c(0,4))

# comparing this to above we see 11/18 - 11/25 our merged data doesnt exist. a lot of outliers in this time period > 2

# what occured between 08/28 - 09/02

ts.plot(ferr_data$op_conc_mg_p_l_hourly)
plot(ferr_data$date, ferr_data$op_conc_mg_p_l_hourly)

# under origina time scale we see the same issue occuring

ferrphos <- phosfax_10m %>%
  filter(date >= ymd("2019-08-15")) %>%
  filter(date <= ymd("2019-09-07"))
plot(ferrphos$date, ferrphos$op_conc_mg_p_l,
     cex = .25,
     main = "Ferric effluent OP",
     ylim = c(0,4))

# numerous values of 0.0004; is this the default error value

# variables that have NA's...dealing with them

final_data$influent_mgd_hourly_avg[4:18]
final_data$daft_sub_gpm[6:17]
final_data$daft_sub_gal[8:21]
final_data$abi_mgd[1:12]
final_data$ras_gpm[1:12]
final_data$mlws_flow_gpm[7:18]

# remove? fill with average, moving average? 

ferr_data %>% ggplot() + 
  geom_point(aes(date, op_mg_p_l), col = "blue") + 
  geom_point(aes(date, op_conc_mg_p_l_hourly), col = "red")

alum_data %>% ggplot() + 
  geom_point(aes(date, op_mg_p_l), col = "blue") + 
  geom_point(aes(date, op_conc_mg_p_l_hourly), col = "red")

# shows linear relationship shifted from effluent being less due to dosing
final_data %>% ggplot() + 
  geom_point(aes(op_mg_p_l, op_conc_mg_p_l_hourly))


alum_melt1 <- melt(alum_data[,c(1,3:4)],id.vars='date', measure.vars=colnames(alum_data[,c(1,3:4)])[-1])
alum_melt2 <- melt(alum_data[,c(1,5:6,10,19:20,22)],id.vars='date', measure.vars=colnames(alum_data[,c(1,5:6,10,19:20,22)])[-1])
alum_melt3 <- melt(alum_data[,c(1,7:8,12)],id.vars='date', measure.vars=colnames(alum_data[,c(1,7:8,12)])[-1])
alum_melt4 <- melt(alum_data[,c(1,14:15)],id.vars='date', measure.vars=colnames(alum_data[,c(1,14:15)])[-1])
alum_melt5 <- melt(alum_data[,c(1,18)],id.vars='date', measure.vars=colnames(alum_data[,c(1,18)])[-1])
alum_melt6 <- melt(alum_data[,c(1,21)],id.vars='date', measure.vars=colnames(alum_data[,c(1,21)])[-1])
alum_melt7 <- melt(alum_data[,c(1,9,11,13)],id.vars='date', measure.vars=colnames(alum_data[,c(1,9,11,13)])[-1])
alum_melt8 <- melt(alum_data[,c(1,16:17)],id.vars='date', measure.vars=colnames(alum_data[,c(1,16:17)])[-1])

# 

ggplot(alum_melt1) +
  geom_boxplot(aes(x=date, y=value, color=variable))

#

ggplot(alum_melt2) +
  geom_boxplot(aes(x=date, y=value, color=variable))

# 

ggplot(alum_melt3) +
  geom_boxplot(aes(x=date, y=value, color=variable))

# 

ggplot(alum_melt4) +
  geom_boxplot(aes(x=date, y=value, color=variable))

# outlier in melt 5. determine how to approach and then replot

ggplot(alum_melt5) +
  geom_boxplot(aes(x=date, y=value, color=variable))

# 

ggplot(alum_melt6) +
  geom_boxplot(aes(x=date, y=value, color=variable))

# consider removing centrate_gal

ggplot(alum_melt7) +
  geom_boxplot(aes(x=date, y=value, color=variable))

ggplot(alum_melt8) +
  geom_boxplot(aes(x=date, y=value, color=variable))



