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

rm( list = ls() )


view(avg_9_to_10_daily)
view(dosing_daily)
view(mixed_liqour_hourly)
view(phosfax_10m)
colnames(avg_9_to_10_daily)


view(avg_9_to_10_daily)

#dosing daily 

#Ferric rows
mix.liq.hrly <-  mix.liq.hrly%>% 
  group_by(date = date(date), hour) %>% 
  ungroup()
mixed.liq.daily <- mix.liq.hrly %>% 
  group_by(date = date(date)) %>% 
  summarise(op_mg_p_l_daily = mean(op_mg_p_l, na.rm = TRUE),
            delta_pct_daily = mean(delta_pct, na.rm = TRUE),
            delta_mg_p_l_daily = mean(detla_mg_p_l, na.rm = TRUE))


mixed_liq_joined <- inner_join(mixed.liq.daily, dosing_daily, by = "date")
view(test)
dim(test)

mixed_liq_joined %>% 
  ggplot(aes(date,op_mg_p_l_daily)) +
  geom_point(aes(col = coagulant))
  
mixed_liq_joined %>% 
  ggplot(aes(date,op_mg_p_l_daily)) +
  geom_boxplot(aes(col = coagulant))




#############################















