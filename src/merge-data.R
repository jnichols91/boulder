# This data comes from the City of Boulder Wastewater Facility. Data cleaning was done by
# Justin Nichols, Arlyn Alcid, and Ocean Wu. June 2020. It was split based on system 
# errors through the months of August 2019 - December 2019


#### Clear environment and load libraries ####
rm( list = ls() )

library(tidyverse)
library(lubridate)

#### Make sure src is the current working directory and load data ####
load("../data/boulderMoWater.rda")


#### Convert 10 minute intervals to hourly average ####
phosfax_hourly <- phosfax_10m %>% 
  mutate( hour = hour(date) ) %>% 
  group_by( date = date(date), hour ) %>% 
  summarise( op_conc_mg_p_l_hourly = mean(op_conc_mg_p_l, na.rm = TRUE) ) %>% 
  ungroup()


#### Turn timestamp into format for merging later ####
phosfax_hourly$date <- with( phosfax_hourly, ymd_h( paste(date, hour, sep= ' ') ) )
phosfax_hourly <- phosfax_hourly %>% select(date, op_conc_mg_p_l_hourly)


#### Truncate the minutes to match the format above ####
mixed_liqour_hourly <- mixed_liqour_hourly %>% 
  mutate( hour = hour(date) ) %>% 
  mutate( date = date(date) )
mixed_liqour_hourly$date <- with( mixed_liqour_hourly, ymd_h( paste(date, hour, sep= ' ') ) )
mixed_liqour_hourly <- mixed_liqour_hourly %>% select(1:2)


#### Remove unnecessary varaibles, per Kate ####
del_vars <- c("influent_mgd_daily_avg", "primary_sludge_gmp_daily_avg", "primary_sludge_gal", "thickened_sludge_gal",
              "gvt_o_f_gpm", "daft_sub_gpm", "daft_sub_gal", "twas_flow_gpm_daily_avg") 
flow_hourly <- flow_hourly %>% 
  select( -all_of(del_vars) ) 


#### Coagulant (as a grouping factor Alum & Ferric) and mols to be merged later ####
dosing_daily_mols <- dosing_daily[,c(1:2,7)]
dosing_daily_mols$coagulant <- as.factor(dosing_daily_mols$coagulant)


#### Merge the mixing data and phosfax hourly #### 
mix_phos <- inner_join(mixed_liqour_hourly, phosfax_hourly, by = "date")

lasso <- inner_join(mix_phos, flow_hourly, by = "date")
mix_phos <- tibble(date = mix_phos$date, phos_change = mix_phos$op_mg_p_l - mix_phos$op_conc_mg_p_l_hourly)

#### Add on the the flow data ####
mix_phos_flow <- inner_join(mix_phos, flow_hourly, by = "date")


#### Separate the timestampe into a date and a time while salvaging data structure for date ####
mix_phos_flow <- mix_phos_flow %>% mutate( date = ymd_hms(date) ) %>% 
  separate(date, into = c('date', 'time'), sep=' ', remove = FALSE) %>% 
  select(date, time, everything())
mix_phos_flow$date <- as_datetime(mix_phos_flow$date)
dosing_daily_mols$date <- as_datetime(dosing_daily_mols$date)

lasso <- lasso %>% mutate( date = ymd_hms(date) ) %>% 
  separate(date, into = c('date', 'time'), sep=' ', remove = FALSE) %>% 
  select(date, time, everything())
lasso$date <- as_datetime(lasso$date)


#### Bring together the coagulant and mols information for a final data set ####
full_ancova <- inner_join(mix_phos_flow, dosing_daily_mols, by = "date") %>% 
  select(date, time, coagulant, everything())
lasso_data <- inner_join(lasso, dosing_daily_mols, by = "date") %>%
  select(date, time, coagulant, op_mg_p_l, everything())


#### Filter out the observations with only Alum and Ferric, i.e., remove 'None' ####
full_ancova$date <- with( full_ancova, ymd_hms( paste(as.character(date), time, sep = ' ') ) )
full_ancova <- full_ancova %>% select(-time) %>%
  filter( coagulant == "Ferric" | coagulant == "Alum" )
lasso_data$date <- with( lasso_data, ymd_hms( paste(as.character(date), time, sep = ' ') ) )
lasso_data <- lasso_data %>% select(-time) %>%
  filter( coagulant == "Ferric" | coagulant == "Alum" )


#### Filled the trend occurring in variable ####
full_ancova$centrate_gal[is.na(full_ancova$centrate_gal)] <- 0
lasso_data$centrate_gal[is.na(lasso_data$centrate_gal)] <- 0

#### Removed days where issues occurred in the system---spotty data ####
full_ancova <- full_ancova %>% 
  filter( date(date) != ymd("2019-08-30") & date(date) != ymd("2019-09-02") ) 
lasso_data <- lasso_data %>% 
  filter( date(date) != ymd("2019-08-30") & date(date) != ymd("2019-09-02") ) 

full_ancova <- full_ancova %>% 
  filter( date(date) < ymd("2019-11-13") | date(date) > ymd("2019-12-01") ) 
lasso_data <- lasso_data %>% 
  filter( date(date) < ymd("2019-11-13") | date(date) > ymd("2019-12-01") ) 

#### Fill NA values based on similar hourly averages from availabel data ####
for ( i in 8:12 ) {
  temp <- full_ancova %>% filter( hour(date) == i & is.na(influent_mgd_hourly_avg) )
  value <- full_ancova %>% 
    filter( hour(date) == i ) %>%  
    select(influent_mgd_hourly_avg) %>% 
    summarise( mean(influent_mgd_hourly_avg, na.rm = TRUE) )
  full_ancova$influent_mgd_hourly_avg[full_ancova$date == temp$date] <- as.numeric(value)
  
  temp <- full_ancova %>% filter( hour(date) == i & is.na(mlws_flow_gpm) )
  value <- full_ancova %>% 
    filter( hour(date) == i ) %>%  
    select(mlws_flow_gpm) %>% 
    summarise( mean(mlws_flow_gpm, na.rm = TRUE) )
  full_ancova$mlws_flow_gpm[full_ancova$date == temp$date] <- as.numeric(value)
  
  temp2 <- lasso_data %>% filter( hour(date) == i & is.na(influent_mgd_hourly_avg) )
  value2 <- lasso_data %>% 
    filter( hour(date) == i ) %>%  
    select(influent_mgd_hourly_avg) %>% 
    summarise( mean(influent_mgd_hourly_avg, na.rm = TRUE) )
  lasso_data$influent_mgd_hourly_avg[lasso_data$date == temp2$date] <- as.numeric(value2)
  
  temp2 <- lasso_data %>% filter( hour(date) == i & is.na(mlws_flow_gpm) )
  value2 <- lasso_data %>% 
    filter( hour(date) == i ) %>%  
    select(mlws_flow_gpm) %>% 
    summarise( mean(mlws_flow_gpm, na.rm = TRUE) )
  lasso_data$mlws_flow_gpm[lasso_data$date == temp2$date] <- as.numeric(value2)
}

for (i in 7:13) {
  temp <- full_ancova %>% filter( hour(date) == i & is.na(abi_mgd) )
  value <- full_ancova %>% 
    filter( hour(date) == i ) %>%  
    select(abi_mgd) %>% 
    summarise( mean(abi_mgd, na.rm = TRUE) )
  full_ancova$abi_mgd[full_ancova$date == temp$date] <- as.numeric(value)
  
  temp <- full_ancova %>% filter( hour(date) == i & is.na(ras_gpm) )
  value <- full_ancova %>% 
    filter( hour(date) == i ) %>%  
    select(ras_gpm) %>% 
    summarise( mean(ras_gpm, na.rm = TRUE) )
  full_ancova$ras_gpm[full_ancova$date == temp$date] <- as.numeric(value)
  
  temp2 <- lasso_data %>% filter( hour(date) == i & is.na(abi_mgd) )
  value2 <- lasso_data %>% 
    filter( hour(date) == i ) %>%  
    select(abi_mgd) %>% 
    summarise( mean(abi_mgd, na.rm = TRUE) )
  lasso_data$abi_mgd[lasso_data$date == temp2$date] <- as.numeric(value2)
  
  temp2 <- lasso_data %>% filter( hour(date) == i & is.na(ras_gpm) )
  value2 <- lasso_data %>% 
    filter( hour(date) == i ) %>%  
    select(ras_gpm) %>% 
    summarise( mean(ras_gpm, na.rm = TRUE) )
  lasso_data$ras_gpm[lasso_data$date == temp2$date] <- as.numeric(value2)
}


#### Filter out two days where issues occured in the morning and remove unimporant sludge variable ####
full_ancova <- full_ancova %>% 
  filter( mlws_flow_gpm > 100 ) %>% 
  select(-twas_flow_gal) %>% 
  select(date, coagulant, mols_of_metal_kmol_day, influent_mgd_hourly_avg, everything())
lasso_data <- lasso_data %>% 
  filter( mlws_flow_gpm > 100 ) %>% 
  select(-twas_flow_gal) %>% 
  select(date, coagulant, op_mg_p_l, everything())

#### Data only including observations a few days into dosing ####
partial_ancova <- full_ancova %>% 
  filter( date(date) > ymd("2019-08-18") ) %>% 
  filter( date(date) != ymd("2019-10-22") ) 
  
#### Save as rda file #### 
save(full_ancova, partial_ancova, file = "../data/final-data.rda")
save(lasso_data, file = "../data/lasso.rda")





