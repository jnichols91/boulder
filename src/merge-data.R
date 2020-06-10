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

# similar as above get into the same format. just include the hour of the day
mixed_liqour_hourly <- mixed_liqour_hourly %>% 
  mutate(hour = hour(date)) %>% 
  mutate(date = date(date))
mixed_liqour_hourly$date <- with(mixed_liqour_hourly, ymd_h(paste(date, hour, sep= ' ')))
mixed_liqour_hourly <- mixed_liqour_hourly %>% select(1:2)

# only grab the day and coagulant to be merged later 
dosing_daily_coag <- dosing_daily[,1:2]
dosing_daily_coag$coagulant <- as.factor(dosing_daily_coag$coagulant)

# flow_hourly already in correct format---start joining data-sets 

temp <- inner_join(mixed_liqour_hourly, phosfax_hourly, by = "date")
temp2 <- inner_join(temp, flow_hourly, by = "date")

# add the coagulant so reseparate the hour and date since dosing is only daily and not hourly... will restructure after
temp2 <- temp2 %>% 
  mutate(hour = hour(date), date = date(date))
final_data <- inner_join(temp2, dosing_daily_coag, by = "date")

# now restructure
final_data$date <- with(final_data, ymd_h(paste(date, hour, sep= ' ')))
# bring the coagulant to the second column 
final_data <- final_data %>% select(date, coagulant, everything()) %>% select(-hour)

save(final_data, file = "../data/final-data.rda")





