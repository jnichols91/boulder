rm( list = ls() )

library(tidyverse)
library(fields)
library(lubridate)
library(latex2exp)

# make sure src is the current working directory

load("../data/final-data.rda")


pct_change <- (final_data$op_conc_mg_p_l_hourly - final_data$op_mg_p_l)/final_data$op_mg_p_l * 100
diff_change <- final_data$op_conc_mg_p_l_hourly - final_data$op_mg_p_l

ancova_data <- tibble(date = final_data$date, diff_change, pct_change, coagulant = final_data$coagulant,
                      time = difftime(final_data$date, final_data$date[1]) / 60)
ancova_data <- ancova_data %>% filter(coagulant == "Ferric" | coagulant == "Alum")
difftime(ancova_data$date, ancova_data$date[1], units = "mins")
