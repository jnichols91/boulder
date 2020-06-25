#### Clear environment and load libraries ####
rm( list = ls() )

library(tidyverse)
library(magrittr)
library(glmnet)
library(latex2exp)
library(pROC)


##### Make sure src is the current working directory and load data ####
load("../data/lasso.rda")


#### Set the general plot them for all plots ####
theme_set(theme_bw())
theme_update(panel.background = element_blank(), axis.line = element_line(colour = "black"))


X.ind.alum <- lasso_data %>% select(-c(op_conc_mg_p_l_hourly, mols_of_metal_kmol_day)) %>%
  filter(coagulant == "Alum") %>% 
  select(-c(date, coagulant))

X.ind.ferric <- lasso_data %>% select(-c(op_conc_mg_p_l_hourly, mols_of_metal_kmol_day)) %>%
  filter(coagulant == "Ferric") %>% 
  select(-c(date, coagulant))


#### ---- Initial Ridge Regression ---- ####

x.alum <- as.matrix(X.ind.alum %>% select(-op_mg_p_l))
y.alum <- as.matrix(X.ind.alum$op_mg_p_l)


set.seed(1)
ridge.alum <- glmnet(x = x.alum, y = y.alum, alpha = 0)
plot(ridge.alum, xvar = "lambda")

#### ----------------------------------- ####

#### ---- Cross Fold Ridge Regression ---- ####

ridge.alum.cv <- cv.glmnet(x = x.alum, y = y.alum,
                           type.measure = "mse",
                           nfold = 4,
                           alpha = 0)


plot(ridge.alum.cv)


best_ridge_coef <- coef(ridge.alum.cv, s = ridge.alum.cv$lambda.min)
ridge.cv.coef.alum<- tibble(name = best_ridge_coef@Dimnames[[1]][best_ridge_coef@i + 1], coefficient = best_ridge_coef@x)
ridge.cv.coef.alum$name<- ridge.cv.coef.alum$name %>% str_replace_all("`", "")
ridge.cv.coef.alum <- ridge.cv.coef.alum[-1,]

#### ---------------------------------- #####



#### ---- Adaptive LASSO ---- ####


adlasso.alum.cv <- cv.glmnet(x = x.alum, y = y.alum,
                  alpha = 1,
                  penalty.factor = 1 / abs(as.numeric(best_ridge_coef)[-1]),
                  type.measure = "mse",
                  nfolds = 4,
                  keep = TRUE)
plot(adlasso.alum.cv)

best_adlass_coef <- coef(adlasso.alum.cv, s = adlasso.alum.cv$lambda.min)

