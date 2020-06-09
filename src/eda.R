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
###############################################
 
test <- inner_join(phos.data.clean, dosing_daily[,1:2], by = c("date"))
