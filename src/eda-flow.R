rm( list = ls() )

library(tidyverse)
library(fields)
library(lubridate)

# make sure src is the current working directory

load("../data/boulderMoWater.rda")
load("../data/phosPrediction.rda")

boxplot(phosfax_10m$op_conc_mg_p_l)
boxplot(mixed_liqour_hourly$op_mg_p_l)
plot(phos.data$op_mg_p_l, phos.data$op_conc_mg_p_l_hourly)
abline( a=0,b=1, col = 'red', lwd = 1, lty=2 )
boxplot(phos.data$centrate_gal)

test <- phos.data.clean[1:57,]

##################################
del <- c("delta_pct", "detla_mg_p_l", "daft_sub_gpm", "daft_sub_gal", "primary_sludge_gmp_hourly_avg", "twas_flow_gpm_hourly_avg")

phos.data.clean <- phos.data %>% select(-all_of(del))
phos.data.clean <- phos.data.clean[15:107,]
phos.data.clean$centrate_gal[is.na(phos.data.clean$centrate_gal)] <- 0

phosfax.10m <- as_tibble(phosfax_10m) %>% 
  mutate(hour = hour(date)) %>% 
  select(date, hour, everything())

ferricphos <- phosfax.10m %>%
  filter(date(date) >= ymd("2019-08-15")) %>%
  filter(date(date) <= ymd("2019-10-20")) 

boxplot(ferricphos$op_conc_mg_p_l)

ferricphos %>%
  filter(op_conc_mg_p_l > 4)

plot(ferricphos$date, ferricphos$op_conc_mg_p_l,
     cex = .25,
     main = "Ferric effluent OP",
     ylim = c(0,3.5),
     col = as.factor(dosing_daily$coagulant[46:112]))

#test2 <- ferricphos %>% filter(op_conc_mg_p_l != .0004)
#plot(test2$date, test2$op_conc_mg_p_l, cex = .25, main = "Ferric effluent OP")


alumphos <- phosfax.10m %>%
  filter(date >= ymd("2019-10-21")) %>%
  filter(date <= ymd("2019-12-15"))

boxplot(alumphos$op_conc_mg_p_l)

alumphos %>%
  filter(op_conc_mg_p_l > 6)

#test <- alumphos[-c(1696:1697, 1709),]
#test %>% filter(op_conc_mg_p_l > 1.5)
#boxplot(test$op_conc_mg_p_l)

plot(alumphos$date, alumphos$op_conc_mg_p_l,
     cex = .25,
     main = "Alum effluent OP",
     ylim = c(0,4))

#test3 <- alumphos %>% filter(op_conc_mg_p_l != .0004)
#plot(test3$date, test3$op_conc_mg_p_l, cex = .25, main = "Alum effluent OP")

ferric <- phos.data.clean %>%
  filter(date(date) >= ymd("2019-08-15")) %>%
  filter(date(date) <= ymd("2019-09-07")) 

plot(ferric$date, ferric$op_mg_p_l)
ts.plot(ferric$op_mg_p_l)

alum <- phos.data.clean %>%
  filter(date >= ymd("2019-10-21")) %>%
  filter(date <= ymd("2019-12-15"))

plot(alum$date, alum$op_mg_p_l)
ts.plot(alum$op_mg_p_l)
####################################################
ferric <- flow_hourly %>%
  filter(date(date) >= ymd("2019-08-15")) %>%
  filter(date(date) <= ymd("2019-10-20"))

#boxplot(ferric$influent_mgd_hourly_avg)
#boxplot(ferric$influent_mgd_daily_avg)
#boxplot(ferric$primary_sludge_gal)
#boxplot(ferric$thickened_sludge_gal)
#boxplot(ferric$abi_mgd)
#boxplot(ferric$daft_sub_gal)

boxplot(ferric$effluent_mgd)
boxplot(ferric$primary_sludge_gmp_hourly_avg)
boxplot(ferric$primary_sludge_gmp_daily_avg)
boxplot(ferric$thickened_sludge_gpm)
boxplot(ferric$gvt_o_f_gpm)
boxplot(ferric$centrate_gal)
boxplot(ferric$daft_sub_gpm)
boxplot(ferric$ras_gpm)
boxplot(ferric$mlws_flow_gpm)
boxplot(ferric$twas_flow_gpm_hourly_avg)
boxplot(ferric$twas_flow_gpm_daily_avg)
boxplot(ferric$twas_flow_gal)

#plot(ferric$date, ferric$influent_mgd_hourly_avg)
#plot(ferric$date, ferric$primary_sludge_gal)
#plot(ferric$date, ferric$abi_mgd)
#plot(ferric$date, ferric$thickened_sludge_gal)
#plot(ferric$date, ferric$influent_mgd_daily_avg)
#plot(ferric$date, ferric$primary_sludge_gmp_daily_avg)
#plot(ferric$date, ferric$abi_mgd)
plot(ferric$date, ferric$effluent_mgd)

ferric %>%
  filter(effluent_mgd < 5)
#why is there a dip in the effluent millions of gallons daily from hours 7-9 on 2019-09-25 

#plot(ferric$date, ferric$primary_sludge_gmp_hourly_avg)
#plot(ferric$date, ferric$thickened_sludge_gpm) 

plot(ferric$date, ferric$gvt_o_f_gpm)
ferric %>% 
  filter(gvt_o_f_gpm < 0)
#why is there a large dip in gvt from hours 11-13 on 2019-09-10
#what is gvt?

plot(ferric$date, ferric$centrate_gal)

ferric %>% 
  filter(centrate_gal > 100000)
#Why is there a spike in centrate gal from hours 15-24 on 2019-09-07

#plot(ferric$date, ferric$daft_sub_gpm)
#plot(ferric$date, ferric$ras_gpm)
#plot(ferric$date, ferric$mlws_flow_gpm)
#plot(ferric$date, ferric$twas_flow_gpm_hourly_avg)
#plot(ferric$date, ferric$twas_flow_gpm_daily_avg)
#plot(ferric$date, ferric$ras_gpm)

ferricphos <- phosfax_10m %>%
  filter(date(date) >= ymd("2019-08-15")) %>%
  filter(date(date) <= ymd("2019-10-20")) 

boxplot(ferricphos$op_conc_mg_p_l)

ferricphos %>%
  filter(op_conc_mg_p_l > 6)
#Outliers in hour 21 on 2019-09-21
#Should we be alarmed by this point b/c it's not actually apart of the ferric dosing data

plot(ferricphos$date, ferricphos$op_conc_mg_p_l,
     cex = .25,
     main = "Ferric effluent OP")

######################################################
alum <- flow_hourly %>%
  filter(date(date) >= ymd("2019-10-21")) %>%
  filter(date(date) <= ymd("2020-01-14"))

alumphos <- phosfax_10m %>%
  filter(date >= ymd("2019-10-21")) %>%
  filter(date <= ymd("2020-01-14"))

plot(alumphos$date, alumphos$op_conc_mg_p_l,
     cex = .25,
     main = "Alum effluent OP",
     ylim = c(0,4))

#No Outliers
#boxplot(alum$influent_mgd_hourly_avg)
#boxplot(alum$influent_mgd_daily_avg)
#boxplot(alum$primary_sludge_gal)
#boxplot(alum$abi_mgd)
#boxplot(alum$effluent_mgd)
#boxplot(alum$primary_sludge_gmp_daily_avg)

#outliers
boxplot(alum$thickened_sludge_gal)
boxplot(alum$primary_sludge_gmp_hourly_avg)
boxplot(alum$thickened_sludge_gpm)
boxplot(alum$gvt_o_f_gpm)
boxplot(alum$centrate_gal)
boxplot(alum$daft_sub_gpm)
boxplot(alum$daft_sub_gal)
boxplot(alum$ras_gpm)
boxplot(alum$mlws_flow_gpm)
boxplot(alum$twas_flow_gpm_hourly_avg)
boxplot(alum$twas_flow_gpm_daily_avg)
boxplot(alum$twas_flow_gal)


#plot(alum$date, alum$influent_mgd_hourly_avg)
plot(alum$date, alum$primary_sludge_gal)
which.max(alum$primary_sludge_gal)
#There is a large jump in the data on 2019-12-07

plot(alum$date, alum$abi_mgd)

plot(alum$date, alum$thickened_sludge_gal)
plot(alum$date, alum$influent_mgd_daily_avg)
plot(alum$date, alum$primary_sludge_gmp_daily_avg)
plot(alum$date, alum$abi_mgd)
plot(alum$date, alum$effluent_mgd)
plot(alum$date, alum$primary_sludge_gmp_hourly_avg)
plot(alum$date, alum$thickened_sludge_gpm)
plot(alum$date, alum$gvt_o_f_gpm)
plot(alum$date, alum$centrate_gal)
plot(alum$date, alum$daft_sub_gpm)
plot(alum$date, alum$ras_gpm)
plot(alum$date, alum$mlws_flow_gpm)
plot(alum$date, alum$twas_flow_gpm_hourly_avg)
plot(alum$date, alum$twas_flow_gpm_daily_avg)

########################################################

final_data %>% 
  ggplot(aes(mlws_flow_gpm, ras_gpm)) +
  geom_point(aes(color = coagulant)) +
  xlab("Mixed Liquor Waste Sludge") +
  ylab("Return Activated Sludge") 


