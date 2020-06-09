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
