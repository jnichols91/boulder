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
pairs(partial_ancova)


#### Influent Covariate ANCOVA Analysis ####
partial_ancova1 <- partial_ancova %>% select(coagulant, phos_change, influent_mgd_hourly_avg)

# shows the 'linear' relationship between the outcome variable and covarient
plot(partial_ancova1$influent_mgd_hourly_avg, partial_ancova1$phos_change)

# plot the points and linear fit by coagulant; test for linearity with covariate
ggplot( partial_ancova1, aes(influent_mgd_hourly_avg, phos_change, color = coagulant) ) +
  geom_point( aes(shape = coagulant), size = 2, alpha = .75 ) +
  geom_smooth( method='lm', se = TRUE ) +
  stat_regline_equation( aes( label = paste(..eq.label.., ..rr.label.., sep = '~~~') ) , size = 3 ) +
  scale_color_brewer(palette = "Dark2") + 
  theme( legend.position = "right",
         axis.text = element_text(size = 12),
         axis.title = element_text(size = 14),
         legend.text = element_text(size = 12),
         legend.title = element_text(size = 14),
         plot.title = element_text( face="bold", size = 16, hjust = 0.5 ) ) +
  labs(x = "Influent Hourly Average (mgd)",
       y = "Change in Phosphorus (mg/L)",
       title = "Influent Vs Change in Phosphorus", 
       color = "Coagulant")  +
  guides(shape = FALSE)

# test for homogeneity; compares the behavior (slope) with the addition of covariate
partial_test1 <- anova_test(phos_change ~ influent_mgd_hourly_avg * coagulant, data = partial_ancova1)
get_anova_table(partial_test1)

# estimate marginal means; anova test with the addition of the covariate---test for p < 0.05
partial_ancova1$coagulant <- as.character(partial_ancova1$coagulant)
em1 <- emmeans_test(partial_ancova1, phos_change ~ coagulant , covariate = influent_mgd_hourly_avg,
                    p.adjust.method = "bonferroni")

# Shapiro Wilk test for normality of residuals
parial_model1 <- lm(phos_change ~ influent_mgd_hourly_avg + coagulant, data = partial_ancova1)
partial_model1.metrics <- augment(parial_model1) %>%
  select(-.hat, -.sigma, -.fitted, -.se.fit) # Remove details

shapiro_test(partial_model1.metrics$.resid) # if not significant assumption is maintained


# Leven's test for homogeneity of variances
partial_model1.metrics %>% levene_test(.resid ~ as.factor(coagulant)) # if not significant assumption is maintained


# outliers 
partial_model1.metrics %>% 
  filter(abs(.std.resid) > 3) %>%
  as.data.frame()

knitr::kable( em1, digits = 3, format = "pandoc", caption = "Influent ANOVA Table" )


#### Mols of metal Covariate ANCOVA Analysis ####
partial_ancova2 <- partial_ancova %>% select(coagulant,phos_change,mols_of_metal_kmol_day)


plot(partial_ancova2$mols_of_metal_kmol_day, partial_ancova2$phos_change)


ggplot( partial_ancova2, aes(mols_of_metal_kmol_day, phos_change, color = coagulant) ) +
  geom_point( aes(shape = coagulant), size = 2, alpha = .75 ) +
  geom_smooth( method='lm', se = TRUE ) +
  stat_regline_equation( aes( label = paste(..eq.label.., ..rr.label.., sep = '~~~') ) , size = 3 ) +
  scale_color_brewer(palette = "Dark2") + 
  theme( legend.position = "right",
         axis.text = element_text(size = 12),
         axis.title = element_text(size = 14),
         legend.text = element_text(size = 12),
         legend.title = element_text(size = 14),
         plot.title = element_text( face="bold", size = 16, hjust = 0.5 ) ) +
  labs(x = "Daily Mols of Metal (Kmol)",
       y = "Change in Phosphorus (mg/L)",
       title = "Mols of Metal Vs Change in Phosphorus", 
       color = "Coagulant")  +
  guides(shape = FALSE)


partial_test2 <- anova_test(phos_change ~ mols_of_metal_kmol_day * coagulant, data = partial_ancova2)
get_anova_table(partial_test2)


partial_ancova2$coagulant <- as.character(partial_ancova2$coagulant)
em2 <- emmeans_test(partial_ancova2, phos_change ~ coagulant , covariate = mols_of_metal_kmol_day,
                    p.adjust.method = "bonferroni")


parial_model2 <- lm(phos_change ~ mols_of_metal_kmol_day + coagulant, data = partial_ancova2)
partial_model2.metrics <- augment(parial_model2) %>%
  select(-.hat, -.sigma, -.fitted, -.se.fit) # Remove details

shapiro_test(partial_model2.metrics$.resid) # if not significant assumption is maintained


partial_model2.metrics %>% levene_test(.resid ~ as.factor(coagulant)) # if not significant assumption is maintained


partial_model2.metrics %>% 
  filter(abs(.std.resid) > 3) %>%
  as.data.frame()


knitr::kable( em2, digits = 3, format = "pandoc", caption = "Mols of Metal ANOVA Table" )


#### Primary Sludge Covariate ANCOVA Analysis ####
partial_ancova3 <- partial_ancova %>% select(coagulant,phos_change,primary_sludge_gmp_hourly_avg)


plot(partial_ancova3$primary_sludge_gmp_hourly_avg, partial_ancova3$phos_change)


ggplot( partial_ancova3, aes(primary_sludge_gmp_hourly_avg, phos_change, color = coagulant) ) +
  geom_point( aes(shape = coagulant), size = 2, alpha = .75 ) +
  geom_smooth( method='lm', se = TRUE ) +
  stat_regline_equation( aes( label = paste(..eq.label.., ..rr.label.., sep = '~~~') ) , size = 3 ) +
  scale_color_brewer(palette = "Dark2") + 
  theme( legend.position = "right",
         axis.text = element_text(size = 12),
         axis.title = element_text(size = 14),
         legend.text = element_text(size = 12),
         legend.title = element_text(size = 14),
         plot.title = element_text( face="bold", size = 16, hjust = 0.5 ) ) +
  labs(x = "Primary Sludge Hourly Average (gmp)",
       y = "Change in Phosphorus (mg/L)",
       title = "Primary Sludge Vs Change in Phosphorus", 
       color = "Coagulant")  +
  guides(shape = FALSE)


partial_test3 <- anova_test(phos_change ~ primary_sludge_gmp_hourly_avg * coagulant, data = partial_ancova3)
get_anova_table(partial_test3)


partial_ancova3$coagulant <- as.character(partial_ancova3$coagulant)
em3 <- emmeans_test(partial_ancova3, phos_change ~ coagulant , covariate = primary_sludge_gmp_hourly_avg,
                    p.adjust.method = "bonferroni")


parial_model3 <- lm(phos_change ~ primary_sludge_gmp_hourly_avg + coagulant, data = partial_ancova3)
partial_model3.metrics <- augment(parial_model3) %>%
  select(-.hat, -.sigma, -.fitted, -.se.fit) # Remove details


shapiro_test(partial_model3.metrics$.resid) # if not significant assumption is maintained


partial_model3.metrics %>% levene_test(.resid ~ as.factor(coagulant)) # if not significant assumption is maintained


partial_model3.metrics %>% 
  filter(abs(.std.resid) > 3) %>%
  as.data.frame()


knitr::kable( em3, digits = 3, format = "pandoc", caption = "Primary Sludge ANOVA Table" )


#### Effluent Flow Covariate ANCOVA Analysis ####
partial_ancova4 <- partial_ancova %>% select(coagulant,phos_change,effluent_mgd)


plot(partial_ancova4$effluent_mgd, partial_ancova4$phos_change)


ggplot( partial_ancova4, aes(effluent_mgd, phos_change, color = coagulant) ) +
  geom_point( aes(shape = coagulant), size = 2, alpha = .75 ) +
  geom_smooth( method='lm', se = TRUE ) +
  stat_regline_equation( aes( label = paste(..eq.label.., ..rr.label.., sep = '~~~') ) , size = 3 ) +
  scale_color_brewer(palette = "Dark2") + 
  theme( legend.position = "right",
         axis.text = element_text(size = 12),
         axis.title = element_text(size = 14),
         legend.text = element_text(size = 12),
         legend.title = element_text(size = 14),
         plot.title = element_text( face="bold", size = 16, hjust = 0.5 ) ) +
  labs(x = "Effluent (mgd)",
       y = "Change in Phosphorus (mg/L)",
       title = "Primary Sludge Vs Change in Phosphorus", 
       color = "Coagulant")  +
  guides(shape = FALSE)


partial_test4 <- anova_test(phos_change ~ effluent_mgd * coagulant, data = partial_ancova4)
get_anova_table(partial_test4)


partial_ancova4$coagulant <- as.character(partial_ancova4$coagulant)
em4 <- emmeans_test(partial_ancova4, phos_change ~ coagulant , covariate = effluent_mgd,
                    p.adjust.method = "bonferroni")


parial_model4 <- lm(phos_change ~ effluent_mgd + coagulant, data = partial_ancova4)
partial_model4.metrics <- augment(parial_model4) %>%
  select(-.hat, -.sigma, -.fitted, -.se.fit) # Remove details


shapiro_test(partial_model4.metrics$.resid) # if not significant assumption is maintained


partial_model4.metrics %>% levene_test(.resid ~ as.factor(coagulant)) # if not significant assumption is maintained


partial_model4.metrics %>% 
  filter(abs(.std.resid) > 3) %>%
  as.data.frame()


knitr::kable( em4, digits = 3, format = "pandoc", caption = "Effluent ANOVA Table" )




