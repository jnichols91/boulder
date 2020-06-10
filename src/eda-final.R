rm( list = ls() )

library(tidyverse)
library(fields)
library(lubridate)
library(latex2exp)
library(reshape2)

# make sure src is the current working directory

load("../data/final-data.rda")
load("../data/boulderMoWater.rda")

# display the number of observations for each type of coagulant

table(final_data$coagulant)

# splitting the data by coagulant 

ferr_data <- final_data %>% filter(coagulant == "Ferric")
alum_data <- final_data %>% filter(coagulant == "Alum")
none_data <- final_data %>% filter(coagulant == "None")

# average was found by hour maybe do a moving average instead?  

# time series plots 

ts.plot(alum_data$op_conc_mg_p_l_hourly)
alum_data %>% ggplot() + 
  geom_point(aes(date, op_conc_mg_p_l_hourly)) + 
  labs(title = "Alum OP Hourly",
       x = "Date",
       y = "Hourly Ortho-phosphate") + 
  theme(plot.title = element_text(face="bold"))

# gap in november looking at this variable at original time scale no average

alumphos <- phosfax_10m %>% # phosfax only alum coagulant every 10 minutes 
  filter(date >= ymd("2019-10-22")) %>%
  filter(date <= ymd("2019-12-15"))

rects1 <- data.frame(xstart = as.POSIXct('2019-11-06 15:00:00'), 
                    xend = as.POSIXct('2019-11-13 00:00:00'))

alumphos %>% ggplot() + 
  geom_point(aes(date, op_conc_mg_p_l), size=1.25) + 
  labs(title = "Alum Effluent OP 10-minute Intervals",
       x = "Date",
       y = "Ortho-phosphate (mg/L)") + 
  geom_rect(data = rects1, aes(xmin = xstart, xmax = xend, 
                              ymin = 0, ymax = 2.5),fill = "red",col = "red", alpha = 0.1) + 
  theme(plot.title = element_text(face="bold"))

# comparing this to above we see 11/18 - 11/25 our merged data doesnt exist. a lot of outliers in this time period > 2

# what occured between 08/28 - 09/02

ts.plot(ferr_data$op_conc_mg_p_l_hourly)
ferr_data %>% ggplot() + 
  geom_point(aes(date, op_conc_mg_p_l_hourly)) + 
  labs(title = "Ferric OP Hourly",
       x = "Date",
       y = "Hourly Ortho-phosphate (mg/L)") + 
  theme(plot.title = element_text(face="bold"))

# under origina time scale we see the same issue occuring

ferrphos <- phosfax_10m %>%
  filter(date >= ymd("2019-08-15")) %>%
  filter(date <= ymd("2019-09-07"))

rects2 <- data.frame(xstart = as.POSIXct('2019-08-30 15:00:00'), 
                     xend = as.POSIXct('2019-09-02 10:00:00'))

ferrphos %>% ggplot() + 
  geom_point(aes(date, op_conc_mg_p_l), size=1.25) + 
  labs(title = "Ferric Effluent OP 10-minute Intervals",
       x = "Date",
       y = "Ortho-phosphate (mg/L)") + 
  geom_rect(data = rects2, aes(xmin = xstart, xmax = xend, 
                              ymin = -0.15, ymax = Inf),fill = "red",col = "red", alpha = 0.1) + 
  theme(plot.title = element_text(face="bold"))
# numerous values of 0.0004; is this the default error value---sensor fault

# variables that have NA's...dealing with them

final_data$influent_mgd_hourly_avg[4:18]
final_data$daft_sub_gpm[6:17]
final_data$daft_sub_gal[8:21]
final_data$abi_mgd[1:12]
final_data$ras_gpm[1:12]
final_data$mlws_flow_gpm[7:18]

# remove? fill with average, moving average? 

ferr_data %>% ggplot() + 
  geom_point(aes(date, op_mg_p_l), col = "blue") + 
  geom_point(aes(date, op_conc_mg_p_l_hourly), col = "red")

alum_data %>% ggplot() + 
  geom_point(aes(date, op_mg_p_l), col = "blue") + 
  geom_point(aes(date, op_conc_mg_p_l_hourly), col = "red")

# shows linear relationship shifted from effluent being less due to dosing
final_data %>% ggplot() + 
  geom_point(aes(op_mg_p_l, op_conc_mg_p_l_hourly))


alum_melt1 <- melt(alum_data[,c(3:4)], measure.vars=colnames(alum_data[,c(3:4)]))
alum_melt2 <- melt(alum_data[,c(5:6,10,19:20,22)], measure.vars=colnames(alum_data[,c(5:6,10,19:20,22)]))
alum_melt3 <- melt(alum_data[,c(7:8,12)], measure.vars=colnames(alum_data[,c(7:8,12)]))
alum_melt4 <- melt(alum_data[,c(14:15)], measure.vars=colnames(alum_data[,c(14:15)]))
alum_melt5 <- melt(alum_data[,c(18)])
alum_melt6 <- melt(alum_data[,c(21)])
alum_melt7 <- melt(alum_data[,c(9,11,13)], measure.vars=colnames(alum_data[,c(9,11,13)]))
alum_melt8 <- melt(alum_data[,c(16:17)], measure.vars=colnames(alum_data[,c(16:17)]))

ferr_melt1 <- melt(ferr_data[,c(3:4)], measure.vars=colnames(ferr_data[,c(3:4)]))
ferr_melt2 <- melt(ferr_data[,c(5:6,10,19:20,22)], measure.vars=colnames(ferr_data[,c(5:6,10,19:20,22)]))
ferr_melt3 <- melt(ferr_data[,c(7:8,12)], measure.vars=colnames(ferr_data[,c(7:8,12)]))
ferr_melt4 <- melt(ferr_data[,c(14:15)], measure.vars=colnames(ferr_data[,c(14:15)]))
ferr_melt5 <- melt(ferr_data[,c(18)])
ferr_melt6 <- melt(ferr_data[,c(21)])
ferr_melt7 <- melt(ferr_data[,c(9,11,13)], measure.vars=colnames(ferr_data[,c(9,11,13)]))
ferr_melt8 <- melt(ferr_data[,c(16:17)], measure.vars=colnames(ferr_data[,c(16:17)]))

ggplot(alum_melt1) +
  geom_boxplot(aes(x = variable, y=value, fill=variable))
ggplot(alum_melt2) +
  geom_boxplot(aes(x = variable, y=value, fill=variable))
ggplot(alum_melt3) +
  geom_boxplot(aes(x = variable, y=value, fill=variable))
ggplot(alum_melt4) +
  geom_boxplot(aes(x = variable, y=value, fill=variable))
ggplot(alum_melt5) +
  geom_boxplot(aes(x="mlws_flow_gpm",y=value, fill="mlws_flow_gpm"))
ggplot(alum_melt6) +
  geom_boxplot(aes(x = "twas_flow_gal", y=value, fill="twas_flow_gal"))
ggplot(alum_melt7) +
  geom_boxplot(aes(x = variable, y=value, fill=variable))
ggplot(alum_melt8) +
  geom_boxplot(aes(x = variable, y=value, fill=variable))

ggplot(ferr_melt1) +
  geom_boxplot(aes(x=variable, y=value, fill=variable))
ggplot(ferr_melt2) +
  geom_boxplot(aes(x = variable, y=value, fill=variable))
ggplot(ferr_melt3) +
  geom_boxplot(aes(x = variable, y=value, fill=variable))
ggplot(ferr_melt4) +
  geom_boxplot(aes(x = variable, y=value, fill=variable))
ggplot(ferr_melt5) +
  geom_boxplot(aes(x="mlws_flow_gpm",y=value, fill="mlws_flow_gpm"))
ggplot(ferr_melt6) +
  geom_boxplot(aes(x = "twas_flow_gal", y=value, fill="twas_flow_gal"))
ggplot(ferr_melt7) +
  geom_boxplot(aes(x = variable, y=value, fill=variable))
ggplot(ferr_melt8) +
  geom_boxplot(aes(x = variable, y=value, fill=variable))

t.test(alum_data$op_conc_mg_p_l_hourly, ferr_data$op_conc_mg_p_l_hourly)

ferr_diff <- ferr_data$op_mg_p_l - ferr_data$op_conc_mg_p_l_hourly
alum_diff <- alum_data$op_mg_p_l - alum_data$op_conc_mg_p_l_hourly




