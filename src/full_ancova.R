#### Clear environment and load libraries ####
rm( list = ls() )

library(tidyverse)
library(lubridate)
library(rstatix)
library(emmeans)
library(ggpubr)


##### Make sure src is the current working directory and load data ####
load("../data/final-data.rda")


#### Set the general plot them for all plots ####
theme_set(theme_bw())
theme_update(panel.background = element_blank(), axis.line = element_line(colour = "black"))


#### Identify linear relationships for possible covariates. See row/column 5 ####
pairs(full_ancova)


full_ancova1 <- full_ancova %>% select(coagulant,phos_change,influent_mgd_hourly_avg)
plot(full_ancova1$influent_mgd_hourly_avg,full_ancova1$phos_change)


full_ancova2 <- full_ancova %>% select(coagulant,phos_change,mols_of_metal_kmol_day)
plot(full_ancova2$mols_of_metal_kmol_day,full_ancova2$phos_change)


full_ancova3 <- full_ancova %>% select(coagulant,phos_change,primary_sludge_gmp_hourly_avg)
plot(full_ancova3$primary_sludge_gmp_hourly_avg,full_ancova3$phos_change)


full_ancova4 <- full_ancova %>% select(coagulant,phos_change,effluent_mgd)
plot(full_ancova4$effluent_mgd,full_ancova4$phos_change)


fit1 <- anova_test(phos_change ~ influent_mgd_hourly_avg * coagulant,data = full_ancova1)
get_anova_table(fit1)

#- equality of error variances across groups, normal and uncorrelated error terms
plot(residuals(lm(phos_change~influent_mgd_hourly_avg+coagulant, data = full_ancova1)))
#- independence between groups and covariate
get_anova_table(anova_test(phos_change ~ influent_mgd_hourly_avg*coagulant, data = full_ancova1))
#- linearity and equality of slopes of different group regression lines

a <- ggplot(full_ancova1, aes(influent_mgd_hourly_avg, phos_change, color = coagulant)) +
  geom_point(aes(shape = coagulant), size = 2, alpha = .75) +
  geom_smooth(method='lm', se = TRUE) +
  stat_regline_equation(aes(label=paste(..eq.label..)), size = 3) +
  scale_color_brewer(palette = "Dark2") + 
  theme(legend.position = "right",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        plot.title = element_text(face="bold", size = 16, hjust = 0.5)) +
  labs(x = "Influent Hourly Average (mgd)",
       y = "Change in Phosphorus (mg/L)",
       title = "Influent Vs Change of Phosphorus", 
       color = "Coagulant")  +
  guides(shape = FALSE)

#png(file = "../plots/week3/full_ancova_influent.png", bg="transparent", width = 1900, height = 700)
#grid.arrange(a, ncol = 1)
#dev.off()

#create anova table in rmarkdown
#knitr::kable(test, digits = 3, format = "pandoc", caption = "ANOVA table")
fit2 <- anova_test(phos_change ~ mols_of_metal_kmol_day * coagulant, data = full_ancova2)
anova_test <- get_anova_table(fit2)

#- equality of error variances across groups, normal and uncorrelated error terms
plot(residuals(lm(phos_change~mols_of_metal_kmol_day+coagulant, data = full_ancova2)))
#- independence between groups and covariate
test <- get_anova_table(anova_test(phos_change ~ mols_of_metal_kmol_day*coagulant, data = full_ancova2))
#- linearity and equality of slopes of different group regression lines

b <- ggplot(full_ancova2, aes(mols_of_metal_kmol_day, phos_change, color = coagulant)) +
  geom_point(aes(shape = coagulant), size = 3, alpha = .75) +
  geom_smooth(method='lm', se = TRUE) +
  stat_regline_equation(aes(label=paste(..eq.label..)), size = 5) +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "right",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        plot.title = element_text(face="bold", size = 16, hjust = 0.5)) +
  labs(x = "Mols of Metal",
       y = "Change in Phosphorus (mg/L)",
       title = "Mols of Metal Vs Change of Phosphorus",
       color = "Coagulant") +
  guides(shape = FALSE)

#png(file = "../plots/week3/full_ancova_mols_metal.png", bg="transparent", width = 1900, height = 700)
#grid.arrange(b, ncol = 1)
#dev.off()

knitr::kable(anova_test, digits = 3, format = "pandoc", caption = "ANOVA table")

fit3 <- anova_test(phos_change ~ primary_sludge_gmp_hourly_avg * coagulant, data = full_ancova3)
get_anova_table(fit3)

#- equality of error variances across groups, normal and uncorrelated error terms
plot(residuals(lm(phos_change~primary_sludge_gmp_hourly_avg+coagulant, data = full_ancova3)))
#- independence between groups and covariate
get_anova_table(anova_test(phos_change ~ primary_sludge_gmp_hourly_avg*coagulant, data = full_ancova3))
#- linearity and equality of slopes of different group regression lines

c <- ggplot(full_ancova3, aes(primary_sludge_gmp_hourly_avg, phos_change, color = coagulant)) +
  geom_point(aes(shape = coagulant), size = 2, alpha = .75) +
  geom_smooth(method='lm', se = TRUE) +
  stat_regline_equation(aes(label=paste(..eq.label..)), size = 3) +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "right",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        plot.title = element_text(face="bold", size = 16, hjust = 0.5)) +
  labs(x = "Primary Sludge Hourly Average (gpm)",
       y = "Change in Phosphorus (mg/L)",
       title = "Primary Sludge Hourly Average Vs Change of Phosphorus",
       color = "Coagulant") +
  guides(shape = FALSE)

#png(file = "../plots/week3/full_ancova_primary.png", bg="transparent", width = 1900, height = 700)
#grid.arrange(c, ncol = 1)
#dev.off()

fit4 <- anova_test(phos_change ~ effluent_mgd * coagulant, data = full_ancova4)
get_anova_table(fit4)

#- equality of error variances across groups, normal and uncorrelated error terms
plot(residuals(lm(phos_change~effluent_mgd+coagulant, data = full_ancova4)))
#- independence between groups and covariate
get_anova_table(anova_test(phos_change ~ effluent_mgd*coagulant, data = full_ancova4))
#- linearity and equality of slopes of different group regression lines


d <- ggplot(full_ancova4, aes(effluent_mgd, phos_change, color = coagulant)) +
  geom_point(aes(shape = coagulant), size = 2, alpha = .75) +
  geom_smooth(method='lm', se = TRUE) +
  stat_regline_equation(aes(label=paste(..eq.label..)), size = 3) +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "right",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        plot.title = element_text(face="bold", size = 16, hjust = 0.5)) +
  labs(x = "Effluent (mgd)",
       y = "Change in Phosphorus",
       title = "Effluent Vs Change of Phosphorus (mg/L)",
       color = "Coagulant") +
  guides(shape = FALSE)

#png(file = "../plots/week3/full_ancova_effluent.png", bg="transparent", width = 1900, height = 700)
#grid.arrange(d, ncol = 1)
#dev.off()

full_ancova5 <- full_ancova %>% select(coagulant,phos_change,
                                       influent_mgd_hourly_avg,
                                       mols_of_metal_kmol_day,
                                       primary_sludge_gmp_hourly_avg,
                                       effluent_mgd)

fit5 <- anova_test(phos_change ~ mols_of_metal_kmol_day +
                     primary_sludge_gmp_hourly_avg +
                     effluent_mgd +
                     coagulant,
                   data = full_ancova5)

get_anova_table(fit5)




