library(fields)
library(dplyr)
library(tidyverse)
library(lubridate)
library(tibble)


load("/Users/haosheng/Desktop/MoWater_Summer_Intern/boulderMoWater.rda")

view(mixed_liqour_hourly)

mixed_liqour_hourly<- mixed_liqour_hourly %>% 
  mutate(hour = hour(date)) %>% 
  group_by(date = date(date), hour)

ferric_mixed_liqour<- mixed_liqour_hourly %>%
  filter(date(date) >= ymd("2019-08-15"))%>% 
  filter(date(date) <= ymd("2019-10-20"))
view(ferric_mixed_liqour)

