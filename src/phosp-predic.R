rm( list = ls() )

library(tidyverse)
library(fields)
library(lubridate)

# make sure src is the current working directory

load("../data/phosPrediction.rda")

del <- c("date", "delta_pct", "detla_mg_p_l", "influent_mgd_daily_avg", "primary_sludge_gmp_daily_avg", "twas_flow_gpm_daily_avg")

phos.data <- phos.data %>% select(-all_of(del))
