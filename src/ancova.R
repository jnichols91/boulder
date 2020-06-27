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


#### Influent Covariate ANCOVA Analysis ####
full_ancova1 <- full_ancova %>% select(coagulant, phos_change, influent_mgd_hourly_avg)


# shows the 'linear' relationship between the outcome variable and covarient
plot(full_ancova1$influent_mgd_hourly_avg, full_ancova1$phos_change)

# plot the points and linear fit by coagulant; test for linearity with covariate
png(file = "~/Desktop/boulder/plots/homogenity_graphs.png")
ggplot( full_ancova1, aes(influent_mgd_hourly_avg, phos_change, color = coagulant) ) +
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
dev.off()
# test for homogeneity; compares the behavior (slope) with the addition of covariate
full_test1 <- anova_test(phos_change ~ influent_mgd_hourly_avg * coagulant, data = full_ancova1)
get_anova_table(full_test1)

# estimate marginal means; anova test with the addition of the covariate---test for p < 0.05
full_ancova1$coagulant <- as.character(full_ancova1$coagulant)
em1 <- emmeans_test(full_ancova1, phos_change ~ coagulant , covariate = influent_mgd_hourly_avg,
             p.adjust.method = "bonferroni")

# Shapiro Wilk test for normality of residuals
model1 <- lm(phos_change ~ influent_mgd_hourly_avg + coagulant, data = full_ancova1)
model1.metrics <- augment(model1) %>%
  select(-.hat, -.sigma, -.fitted, -.se.fit) # Remove details

shapiro_test(model1.metrics$.resid) # if not significant assumption is maintained


# Leven's test for homogeneity of variances
model1.metrics %>% levene_test(.resid ~ as.factor(coagulant)) # if not significant assumption is maintained


# outliers 
model1.metrics %>% 
  filter(abs(.std.resid) > 3) %>%
  as.data.frame()

knitr::kable( em1, digits = 3, format = "pandoc", caption = "Influent ANOVA Table" )


#### Mols of metal Covariate ANCOVA Analysis ####
full_ancova2 <- full_ancova %>% select(coagulant, phos_change, mols_of_metal_kmol_day)


plot(full_ancova2$mols_of_metal_kmol_day, full_ancova2$phos_change)

png(file = "~/Desktop/boulder/plots/homogenity_graphs.png")
ggplot( full_ancova2, aes(mols_of_metal_kmol_day, phos_change, color = coagulant) ) +
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
dev.off()

full_test2 <- anova_test(phos_change ~ mols_of_metal_kmol_day * coagulant, data = full_ancova2)
get_anova_table(full_test2)


full_ancova2$coagulant <- as.character(full_ancova2$coagulant)
em2 <- emmeans_test(full_ancova2, phos_change ~ coagulant , covariate = mols_of_metal_kmol_day,
                    p.adjust.method = "bonferroni")


model2 <- lm(phos_change ~ mols_of_metal_kmol_day + coagulant, data = full_ancova2)
model2.metrics <- augment(model2) %>%
  select(-.hat, -.sigma, -.fitted, -.se.fit) # Remove details

shapiro_test(model2.metrics$.resid) # if not significant assumption is maintained


model2.metrics %>% levene_test(.resid ~ as.factor(coagulant)) # if not significant assumption is maintained


model2.metrics %>% 
  filter(abs(.std.resid) > 3) %>%
  as.data.frame()


knitr::kable( em2, digits = 5, format = "pandoc", caption = "Mols of Metal ANOVA Table" )


#### Primary Sludge Covariate ANCOVA Analysis ####
full_ancova3 <- full_ancova %>% select(coagulant, phos_change, primary_sludge_gmp_hourly_avg)


plot(full_ancova3$primary_sludge_gmp_hourly_avg, full_ancova3$phos_change)


ggplot( full_ancova3, aes(primary_sludge_gmp_hourly_avg, phos_change, color = coagulant) ) +
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
  labs(x = "Hourly Primary Sludge (gpm)",
       y = "Change in Phosphorus (mg/L)",
       title = "Primary Sludge Vs Change in Phosphorus", 
       color = "Coagulant")  +
  guides(shape = FALSE)


full_test3 <- anova_test(phos_change ~ primary_sludge_gmp_hourly_avg * coagulant, data = full_ancova3)
get_anova_table(full_test3)


full_ancova3$coagulant <- as.character(full_ancova3$coagulant)
em3 <- emmeans_test(full_ancova3, phos_change ~ coagulant , covariate = primary_sludge_gmp_hourly_avg,
                    p.adjust.method = "bonferroni")

model3 <- lm(phos_change ~ primary_sludge_gmp_hourly_avg + coagulant, data = full_ancova3)
model3.metrics <- augment(model3) %>%
  select(-.hat, -.sigma, -.fitted, -.se.fit) # Remove details

shapiro_test(model3.metrics$.resid) # if not significant assumption is maintained


model3.metrics %>% levene_test(.resid ~ as.factor(coagulant)) # if not significant assumption is maintained


model3.metrics %>% 
  filter(abs(.std.resid) > 3) %>%
  as.data.frame()


knitr::kable( em3, digits = 3, format = "pandoc", caption = "Primary Sludge ANOVA Table" )


#### Effluent Flow Covariate ANCOVA Analysis ####
full_ancova4 <- full_ancova %>% select(coagulant, phos_change, effluent_mgd)

plot(full_ancova4$effluent_mgd, full_ancova4$phos_change)


ggplot( full_ancova4, aes(effluent_mgd, phos_change, color = coagulant) ) +
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
       title = "Effluent Vs Change in Phosphorus", 
       color = "Coagulant")  +
  guides(shape = FALSE)


full_test4 <- anova_test(phos_change ~ effluent_mgd * coagulant, data = full_ancova4)
get_anova_table(full_test4)


full_ancova4$coagulant <- as.character(full_ancova4$coagulant)
em4 <- emmeans_test(full_ancova4, phos_change ~ coagulant , covariate = effluent_mgd,
                    p.adjust.method = "bonferroni")


model4 <- lm(phos_change ~ effluent_mgd + coagulant, data = full_ancova4)
model4.metrics <- augment(model4) %>%
  select(-.hat, -.sigma, -.fitted, -.se.fit) # Remove details

shapiro_test(model4.metrics$.resid) # if not significant assumption is maintained


model4.metrics %>% levene_test(.resid ~ as.factor(coagulant)) # if not significant assumption is maintained


model4.metrics %>% 
  filter(abs(.std.resid) > 3) %>%
  as.data.frame()


knitr::kable(em4, digits = 3, format = "pandoc", caption = "Effluent ANOVA Table" )

######################## Full ancova 5 with all 5 covariates
full_ancova5 <- full_ancova %>% select(coagulant, phos_change, effluent_mgd,
                                       primary_sludge_gmp_hourly_avg,
                                       influent_mgd_hourly_avg,
                                       mols_of_metal_kmol_day)

plot(full_ancova4$effluent_mgd, full_ancova4$phos_change)


#ggplot( full_ancova, aes(effluent_mgd, phos_change, color = coagulant) ) +
 # geom_point( aes(shape = coagulant), size = 2, alpha = .75 ) +
  #geom_smooth( method='lm', se = TRUE ) +
 # stat_regline_equation( aes( label = paste(..eq.label.., ..rr.label.., sep = '~~~') ) , size = 3 ) +
  #scale_color_brewer(palette = "Dark2") + 
  #theme( legend.position = "right",
   #      axis.text = element_text(size = 12),
   #      axis.title = element_text(size = 14),
  #       legend.text = element_text(size = 12),
  #       legend.title = element_text(size = 14),
  #       plot.title = element_text( face="bold", size = 16, hjust = 0.5 ) ) +
  #labs(x = "Effluent (mgd)",
 #      y = "Change in Phosphorus (mg/L)",
  #     title = "Effluent Vs Change in Phosphorus", 
  #     color = "Coagulant")  +
 # guides(shape = FALSE)


full_test5 <- anova_test(phos_change ~ (effluent_mgd + 
                                          primary_sludge_gmp_hourly_avg +
                                        influent_mgd_hourly_avg +
                                        mols_of_metal_kmol_day)* coagulant, data = full_ancova5)
get_anova_table(full_test5)


full_ancova5$coagulant <- as.character(full_ancova4$coagulant)
em5 <- emmeans_test(full_ancova5, phos_change ~ coagulant , covariate = 
                      c(effluent_mgd, 
                      primary_sludge_gmp_hourly_avg,
                      influent_mgd_hourly_avg,
                      mols_of_metal_kmol_day)
                    p.adjust.method = "bonferroni")


model4 <- lm(phos_change ~ effluent_mgd + coagulant, data = full_ancova4)
model4.metrics <- augment(model4) %>%
  select(-.hat, -.sigma, -.fitted, -.se.fit) # Remove details

shapiro_test(model4.metrics$.resid) # if not significant assumption is maintained


model4.metrics %>% levene_test(.resid ~ as.factor(coagulant)) # if not significant assumption is maintained


model4.metrics %>% 
  filter(abs(.std.resid) > 3) %>%
  as.data.frame()


knitr::kable(em4, digits = 3, format = "pandoc", caption = "Effluent ANOVA Table" )




#### Saving plots ####
# png(file = "../plots/week3/full_ancova_influent.png", bg="transparent", width = 1900, height = 700)
# g1 or any other assigned plot
# dev.off()


#- equality of error variances across groups, normal and uncorrelated error terms
plot(residuals(lm(phos_change~mols_of_metal_kmol_day+coagulant, data = full_ancova2)))
#- independence between groups and covariate
test <- get_anova_table(anova_test(phos_change ~ mols_of_metal_kmol_day*coagulant, data = full_ancova2))
#- linearity and equality of slopes of different group regression lines





