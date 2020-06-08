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
view(lab_daily)

lab_daily %>% ggplot(aes(date,))

view(avg_9_to_10_daily)


mix.liq.hrly <-  mix.liq.hrly%>% 
  group_by(date = date(date), hour) %>% 
  ungroup()
mixed.liq.daily <- mix.liq.hrly %>% 
  group_by(date = date(date)) %>% 
  summarise(op_mg_p_l_daily = mean(op_mg_p_l, na.rm = TRUE),
            delta_pct_daily = mean(delta_pct, na.rm = TRUE),
            delta_mg_p_l_daily = mean(detla_mg_p_l, na.rm = TRUE))

phosfax_10m_daily <- phosfax_10m %>% 
  mutate(hour = hour(date)) %>% 
  group_by( date = date(date),hour) %>% 
  summarise( op_conc_mg_p_l_daily = mean(op_conc_mg_p_l))
view(phosfax_10m_daily)

mixed_liq_joined <- inner_join(mixed.liq.daily, dosing_daily, by = "date")

dosing_phosfax10m_joined <- inner_join(dosing_daily,phosfax_10m_daily, by = "date")

view(dosing_phosfax10m_joined)


mixed_liq_joined %>% 
  ggplot(aes(date,op_mg_p_l_daily)) +
  geom_point(aes(col = coagulant))

# boxplot of mixed_liq_joined
mixed_liq_joined %>% 
  ggplot(aes(date,op_mg_p_l_daily)) +
  geom_boxplot(aes(col = coagulant))

# phosfax point
dosing_phosfax10m_joined %>% 
  ggplot(aes(date, op_conc_mg_p_l_daily)) +
  geom_point(aes(col = coagulant)) +
  geom_smooth(method = "loess")

# phosfax density
dosing_phosfax10m_joined %>%
  ggplot(aes(date,op_conc_mg_p_l_daily)) +
  geom_density(aes(op_conc_mg_p_l_daily))
  
#############################















