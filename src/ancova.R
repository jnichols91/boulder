rm( list = ls() )


library(tidyverse)
library(fields)
library(lubridate)
library(latex2exp)
library(rstatix)
install.packages("emmeans")
library(emmeans)

load("../data/final-data.rda")

pairs(full_ancova)


full_ancova1 <- full_ancova %>% select(coagulant,phos_change,influent_mgd_hourly_avg)
plot(full_ancova1$influent_mgd_hourly_avg,full_ancova1$phos_change)


full_ancova2 <- full_ancova %>% select(coagulant,phos_change,mols_of_metal_kmol_day)
plot(full_ancova2$mols_of_metal_kmol_day,full_ancova2$phos_change)


full_ancova3 <- full_ancova %>% select(coagulant,phos_change,primary_sludge_gmp_hourly_avg)
plot(full_ancova3$primary_sludge_gmp_hourly_avg,full_ancova3$phos_change)


full_ancova4 <- full_ancova %>% select(coagulant,phos_change,effluent_mgd)
plot(full_ancova4$effluent_mgd,full_ancova4$phos_change)

fit1 <- anova_test(phos_change ~ influent_mgd_hourly_avg + coagulant,data = full_ancova1)
get_anova_table(fit1)

fit2 <- anova_test(phos_change ~ mols_of_metal_kmol_day + coagulant, data = full_ancova2)
get_anova_table(fit2)

fit3 <- anova_test(phos_change ~ primary_sludge_gmp_hourly_avg + coagulant, data = full_ancova3)
get_anova_table(fit3)

fit4 <- anova_test(phos_change ~ effluent_mgd + coagulant, data = full_ancova4)
get_anova_table(fit4)
