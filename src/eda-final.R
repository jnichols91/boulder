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
# thursday plot 1
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

ferr_data %>% ggplot() + 
  geom_point(aes(date, op_mg_p_l), col = "blue") + 
  geom_point(aes(date, op_conc_mg_p_l_hourly), col = "red")

alum_data %>% ggplot() + 
  geom_point(aes(date, op_mg_p_l), col = "blue") + 
  geom_point(aes(date, op_conc_mg_p_l_hourly), col = "red")

# shows linear relationship shifted from effluent being less due to dosing
final_data %>% ggplot() + 
  geom_point(aes(op_mg_p_l, op_conc_mg_p_l_hourly))

ferr_data <- ferr_data %>% mutate(hour = hour(date))
alum_data <- alum_data %>% mutate(hour = hour(date))

alum_melt1 <- melt(alum_data[,c(8,15)], id.vars = "hour")
alum_melt2 <- melt(alum_data[,c(13,15)], id.vars = "hour")
alum_melt3 <- melt(alum_data[,c(9:10,15)], id.vars = "hour")
alum_melt4 <- melt(alum_data[,c(6,11,15)], id.vars = "hour")
alum_melt5 <- melt(alum_data[,c(3:4,15)], id.vars = "hour")
alum_melt6 <- melt(alum_data[,c(5,7,12,14,15)], id.vars = "hour")

ferr_melt1 <- melt(alum_data[,c(8,15)], id.vars = "hour")
ferr_melt2 <- melt(alum_data[,c(13,15)], id.vars = "hour")
ferr_melt3 <- melt(alum_data[,c(9:10,15)], id.vars = "hour")
ferr_melt4 <- melt(alum_data[,c(6,11,15)], id.vars = "hour")
ferr_melt5 <- melt(alum_data[,c(3:4,15)], id.vars = "hour")
ferr_melt6 <- melt(alum_data[,c(5,7,12,14,15)], id.vars = "hour")

ggplot(alum_melt1) +
  geom_boxplot(aes(y=value, fill=variable), outlier.size = 0.5, outlier.color = "red") + 
  facet_wrap(vars(hour)) + 
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank())
ggplot(alum_melt2) +
  geom_boxplot(aes(y=value, fill=variable), outlier.size = 0.5, outlier.color = "red") + 
  facet_wrap(vars(hour)) + 
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank())
ggplot(alum_melt3) +
  geom_boxplot(aes(y=value, fill=variable), outlier.size = 0.5, outlier.color = "red") + 
  facet_wrap(vars(hour)) + 
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank())
ggplot(alum_melt4) +
  geom_boxplot(aes(y=value, fill=variable), outlier.size = 0.5, outlier.color = "red") + 
  facet_wrap(vars(hour)) + 
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank())
ggplot(alum_melt5) +
  geom_boxplot(aes(y=value, fill=variable), outlier.size = 0.5, outlier.color = "red") + 
  facet_wrap(vars(hour)) + 
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank())
ggplot(alum_melt6) +
  geom_boxplot(aes(y=value, fill=variable), outlier.size = 0.5, outlier.color = "red") + 
  facet_wrap(vars(hour)) + 
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank())

ggplot(ferr_melt1) +
  geom_boxplot(aes(y=value, fill=variable), outlier.size = 0.5, outlier.color = "red") + 
  facet_wrap(vars(hour)) + 
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank())
ggplot(ferr_melt2) +
  geom_boxplot(aes(y=value, fill=variable), outlier.size = 0.5, outlier.color = "red") + 
  facet_wrap(vars(hour)) + 
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank())
ggplot(ferr_melt3) +
  geom_boxplot(aes(y=value, fill=variable), outlier.size = 0.5, outlier.color = "red") + 
  facet_wrap(vars(hour)) + 
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank())
ggplot(ferr_melt4) +
  geom_boxplot(aes(y=value, fill=variable), outlier.size = 0.5, outlier.color = "red") + 
  facet_wrap(vars(hour)) + 
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank())
ggplot(ferr_melt5) +
  geom_boxplot(aes(y=value, fill=variable), outlier.size = 0.5, outlier.color = "red") + 
  facet_wrap(vars(hour)) + 
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank())
ggplot(ferr_melt6) +
  geom_boxplot(aes(y=value, fill=variable), outlier.size = 0.5, outlier.color = "red") + 
  facet_wrap(vars(hour)) + 
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank())

# the percent change in op levels 

ferr_diff_pct <- (ferr_data$op_mg_p_l - ferr_data$op_conc_mg_p_l_hourly)/ferr_data$op_mg_p_l * 100
alum_diff_pct <- (alum_data$op_mg_p_l - alum_data$op_conc_mg_p_l_hourly)/alum_data$op_mg_p_l * 100

# shows that alum can be concluded as better
t.test(alum_diff_pct, ferr_diff_pct)




