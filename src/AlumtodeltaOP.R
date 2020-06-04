library(fields)
library(dplyr)
library(tidyverse)
library(lubridate)
library(tibble)

load("/Users/haosheng/Desktop/MoWater_Summer_Intern/boulderMoWater.rda")

#1	avg_9_to_10_daily it could be irrelevant 
#2	dosing_daily
#3	flow_hourly
#4	lab_daily 
#5	mixed_liqour_hourly
#6	phosfax_10m
#7	total_flow_daily


rm(list = list())



list()view(data)


view(avg_9_to_10_daily)
view(dosing_daily)
view(mixed_liqour_hourly)
view(phosfax_10m)
colnames(avg_9_to_10_daily)


view(avg_9_to_10_daily)

#dosing daily 
as.numeric()

which(avg_9_to_10_daily$date == "2019-07-01")



length(which(dosing_daily$coagulant == "Alum"))
length(dosing_daily$)

