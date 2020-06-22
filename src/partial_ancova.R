rm( list = ls() )


library(tidyverse)
library(fields)
library(lubridate)
library(latex2exp)
library(rstatix)
library(emmeans)
library(ggpubr)

load("../data/final-data.rda")

theme_set(theme_bw())
theme_update(panel.background = element_blank(), axis.line = element_line(colour = "black"))

#Partial Ancova is partial Ancova

###Covariate1 = Influent Hourly Average
pairs(partial_ancova)

partial_ancova1 <- partial_ancova %>% select(coagulant,phos_change,influent_mgd_hourly_avg)
plot(partial_ancova1$influent_mgd_hourly_avg,partial_ancova1$phos_change)

fit1 <- anova_test(phos_change ~ influent_mgd_hourly_avg * coagulant,data = partial_ancova1)
get_anova_table(fit1)

#- equality of error variances across groups, normal and uncorrelated error terms
plot(residuals(lm(phos_change~influent_mgd_hourly_avg+coagulant, data = partial_ancova1)))
#- independence between groups and covariate
get_anova_table(anova_test(phos_change ~ influent_mgd_hourly_avg*coagulant, data = partial_ancova1))
#- linearity and equality of slopes of different group regression lines

ggplot(partial_ancova1, aes(influent_mgd_hourly_avg, phos_change, color = coagulant)) +
  geom_point(aes(shape = coagulant), size = 2, alpha = .75) +
  geom_smooth(method='lm', se = FALSE) +
  stat_regline_equation(aes(label=paste(..eq.label.., ..rr.label.., sep = "~~~")), size = 3) +
  scale_color_brewer(palette = "Dark2") + 
  theme(legend.position = "bottom") +
  labs(x = "Influent Hourly Average (mgd)",
       y = "Change in Phosphorus",
       title = "Influent Vs Change of Phosphorus",
       color = "Coagulant") +
  guides(shape = FALSE)

partial_ancova1$coagulant <- as.character(partial_ancova1$coagulant)
emmeans_test(partial_ancova1, phos_change ~ coagulant , covariate = influent_mgd_hourly_avg,
             p.adjust.method = "bonferroni")


###Covariate2 = mols_of_metal_kmol_day


partial_ancova2 <- partial_ancova %>% select(coagulant,phos_change,mols_of_metal_kmol_day)
plot(partial_ancova2$mols_of_metal_kmol_day,partial_ancova2$phos_change)

fit2 <- anova_test(phos_change ~ mols_of_metal_kmol_day * coagulant,data = partial_ancova2)
get_anova_table(fit2)

#- equality of error variances across groups, normal and uncorrelated error terms
plot(residuals(lm(phos_change~mols_of_metal_kmol_day+coagulant, data = partial_ancova2)))
#- independence between groups and covariate
get_anova_table(anova_test(phos_change ~ mols_of_metal_kmol_day*coagulant, data = partial_ancova2))
#- linearity and equality of slopes of different group regression lines

ggplot(partial_ancova2, aes(mols_of_metal_kmol_day, phos_change, color = coagulant)) +
  geom_point(aes(shape = coagulant), size = 2, alpha = .75) +
  geom_smooth(method='lm', se = FALSE) +
  stat_regline_equation(aes(label=paste(..eq.label.., ..rr.label.., sep = "~~~")), size = 3) +
  scale_color_brewer(palette = "Dark2") + 
  theme(legend.position = "bottom") +
  labs(x = "Mols of Metal Daily (Kmol)",
       y = "Change in Phosphorus",
       title = "Mols of Metal Vs Change of Phosphorus",
       color = "Coagulant") +
  guides(shape = FALSE)


partial_ancova2$coagulant <- as.character(partial_ancova2$coagulant)
emmeans_test(partial_ancova2, phos_change ~ coagulant , covariate = mols_of_metal_kmol_day,
             p.adjust.method = "bonferroni")

## Covariate3 = primary_sludge_gmp_hourly_avg


partial_ancova3 <- partial_ancova %>% select(coagulant,phos_change,primary_sludge_gmp_hourly_avg)
plot(partial_ancova3$primary_sludge_gmp_hourly_avg,partial_ancova3$phos_change)

fit3 <- anova_test(phos_change ~ primary_sludge_gmp_hourly_avg * coagulant,data = partial_ancova3)
get_anova_table(fit3)

#- equality of error variances across groups, normal and uncorrelated error terms
plot(residuals(lm(phos_change~primary_sludge_gmp_hourly_avg+coagulant, data = partial_ancova3)))
#- independence between groups and covariate
get_anova_table(anova_test(phos_change ~ primary_sludge_gmp_hourly_avg*coagulant, data = partial_ancova3))
#- linearity and equality of slopes of different group regression lines

ggplot(partial_ancova3, aes(primary_sludge_gmp_hourly_avg, phos_change, color = coagulant)) +
  geom_point(aes(shape = coagulant), size = 2, alpha = .75) +
  geom_smooth(method='lm', se = FALSE) +
  stat_regline_equation(aes(label=paste(..eq.label..,..rr.label.., sep = "~~~")), size = 3) +
  scale_color_brewer(palette = "Dark2") + 
  theme(legend.position = "bottom") +
  labs(x = "Primary Sludge Hourly Average (gpm)",
       y = "Change in Phosphorus",
       title = "Primary Sludge Vs Change of Phosphorus",
       color = "Coagulant") +
  guides(shape = FALSE)


partial_ancova3$coagulant <- as.character(partial_ancova3$coagulant)
emmeans_test(partial_ancova3, phos_change ~ coagulant , covariate = primary_sludge_gmp_hourly_avg,
             p.adjust.method = "bonferroni")

## Covariate4 = Effluent


partial_ancova4 <- partial_ancova %>% select(coagulant,phos_change,effluent_mgd)
plot(partial_ancova4$effluent_mgd,partial_ancova4$phos_change)

fit4 <- anova_test(phos_change ~ effluent_mgd * coagulant,data = partial_ancova4)
get_anova_table(fit4)

#- equality of error variances across groups, normal and uncorrelated error terms
plot(residuals(lm(phos_change~effluent_mgd+coagulant, data = partial_ancova4)))
#- independence between groups and covariate
get_anova_table(anova_test(phos_change ~ effluent_mgd*coagulant, data = partial_ancova4))
#- linearity and equality of slopes of different group regression lines

ggplot(partial_ancova4, aes(effluent_mgd, phos_change, color = coagulant)) +
  geom_point(aes(shape = coagulant), size = 2, alpha = .75) +
  geom_smooth(method='lm', se = FALSE) +
  stat_regline_equation(aes(label=paste(..eq.label..,..rr.label.., sep = "~~~")), size = 3) +
  scale_color_brewer(palette = "Dark2") + 
  theme(legend.position = "bottom") +
  labs(x = "Effluent (mgd)",
       y = "Change in Phosphorus",
       title = "Effluent Vs Change of Phosphorus",
       color = "Coagulant") +
  guides(shape = FALSE)

partial_ancova4$coagulant <- as.character(partial_ancova4$coagulant)
emmeans_test(partial_ancova4, phos_change ~ coagulant , covariate = effluent_mgd,
             p.adjust.method = "bonferroni")

## Covariate5 = All Combined 

partial_ancova5 <- partial_ancova %>% select(coagulant,phos_change,
                                       influent_mgd_hourly_avg,
                                       mols_of_metal_kmol_day,
                                       primary_sludge_gmp_hourly_avg,
                                       effluent_mgd)


fit5 <- anova_test(phos_change ~ influent_mgd_hourly_avg +
                     mols_of_metal_kmol_day +
                     primary_sludge_gmp_hourly_avg +
                     effluent_mgd +
                     coagulant,
                   data = partial_ancova5)

get_anova_table(fit5)

partial_ancova5$coagulant <- as.character(partial_ancova5$coagulant)
emmeans_test(partial_ancova5, phos_change ~ coagulant , covariate = c(influent_mgd_hourly_avg,
             mols_of_metal_kmol_day,
             primary_sludge_gmp_hourly_avg,
             effluent_mgd),
             p.adjust.method = "bonferroni")

####Which is the correct way to run the Ancova Test ?

fit6 <- anova_test(phos_change ~ c(influent_mgd_hourly_avg *
                     mols_of_metal_kmol_day *
                     primary_sludge_gmp_hourly_avg *
                     effluent_mgd)  +
                     coagulant,
                   data = partial_ancova5)

get_anova_table(fit6)


fit7 <- anova_test(phos_change ~ c(influent_mgd_hourly_avg +
                     mols_of_metal_kmol_day +
                     primary_sludge_gmp_hourly_avg +
                     effluent_mgd) +
                     coagulant,
                   data = partial_ancova5)

get_anova_table(fit7)
