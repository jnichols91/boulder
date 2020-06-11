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
final_data$centrate_gal[100:101] <- 0

# morning talk thursday
# sensor issues refer to plot of red boxed ferric area 

final_data <- final_data %>% 
  filter(date(date) != ymd("2019-08-30") & date(date) != ymd("2019-09-02")) # plot insert number ferric issue

final_data <- final_data %>% 
  filter(date(date) < ymd("2019-11-13") | date(date) > ymd("2019-12-01")) # plot alum issue

# vars Kate recommended to remove 
del_vars <- c("influent_mgd_daily_avg", "primary_sludge_gmp_daily_avg", "primary_sludge_gal", "thickened_sludge_gal",
              "gvt_o_f_gpm", "daft_sub_gpm", "daft_sub_gal", "twas_flow_gpm_daily_avg") 

final_data <- final_data %>% 
  select(-all_of(del_vars)) 

# look at NA's and mention approach 

for (i in 8:12) {
  temp <- final_data %>% filter(hour(date) == i & is.na(influent_mgd_hourly_avg))
  value <- final_data %>% 
    filter(hour(date) == i) %>%  
    select(influent_mgd_hourly_avg) %>% 
    summarise(mean(influent_mgd_hourly_avg, na.rm = TRUE))
  final_data$influent_mgd_hourly_avg[final_data$date == temp$date] <- as.numeric(value)
  
  temp <- final_data %>% filter(hour(date) == i & is.na(mlws_flow_gpm))
  value <- final_data %>% 
    filter(hour(date) == i) %>%  
    select(mlws_flow_gpm) %>% 
    summarise(mean(mlws_flow_gpm, na.rm = TRUE))
  final_data$mlws_flow_gpm[final_data$date == temp$date] <- as.numeric(value)
}

for (i in 7:13) {
  temp <- final_data %>% filter(hour(date) == i & is.na(abi_mgd))
  value <- final_data %>% 
    filter(hour(date) == i) %>%  
    select(abi_mgd) %>% 
    summarise(mean(abi_mgd, na.rm = TRUE))
  final_data$abi_mgd[final_data$date == temp$date] <- as.numeric(value)
  
  temp <- final_data %>% filter(hour(date) == i & is.na(ras_gpm))
  value <- final_data %>% 
    filter(hour(date) == i) %>%  
    select(ras_gpm) %>% 
    summarise(mean(ras_gpm, na.rm = TRUE))
  final_data$ras_gpm[final_data$date == temp$date] <- as.numeric(value)
}

final_data <- final_data %>% 
  filter(mlws_flow_gpm > 100) # SCADA system down 

ferr_data <- final_data %>% filter(coagulant == "Ferric")
alum_data <- final_data %>% filter(coagulant == "Alum")

# save(final_data, ferr_data, alum_data, file = "../data/final-data.rda")





