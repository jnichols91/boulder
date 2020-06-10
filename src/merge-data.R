rm( list = ls() )

library(tidyverse)
library(fields)
library(lubridate)
library(latex2exp)

# make sure src is the current working directory

load("../data/boulderMoWater.rda")

# add the hour for grouping and averaging hourly 
phosfax_hourly <- phosfax_10m %>% 
  mutate(hour = hour(date)) %>% 
  group_by(date = date(date), hour) %>% 
  summarise(op_conc_mg_p_l_hourly = mean(op_conc_mg_p_l, na.rm = TRUE)) %>% 
  ungroup()

# turn the date back into one posixct object and eliminate the hour column
phosfax_hourly$date <- with(phosfax_hourly, ymd_h(paste(date, hour, sep= ' ')))
phosfax_hourly <- phosfax_hourly %>% select(date, op_conc_mg_p_l_hourly)








