rm( list = ls() )

library(tidyverse)
library(fields)
library(lubridate)

# make sure src is the current working directory

load("../data/boulderMoWater.rda")

flow.hrly <- as_tibble(flow_hourly) %>% 
  mutate(hour = hour(date)) %>% 
  select(date, hour, everything())
mix.liq.hrly <- as_tibble(mixed_liqour_hourly) %>% 
  mutate(hour = hour(date)) %>% 
  select(date, hour, everything())
phosfax.10m <- as_tibble(phosfax_10m) %>% 
  mutate(hour = hour(date)) %>% 
  select(date, hour, everything())

flow.hrly <- flow.hrly %>% 
  group_by(date = date(date), hour)
mix.liq.hrly <- mix.liq.hrly %>% 
  group_by(date = date(date), hour) 
phosfax.hrly <- phosfax.10m %>% 
  group_by(date = date(date), hour) %>% 
  summarise(op_conc_mg_p_l_hourly = mean(op_conc_mg_p_l))



