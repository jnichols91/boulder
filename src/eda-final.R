rm( list = ls() )

library(tidyverse)
library(fields)
library(lubridate)
library(latex2exp)

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






