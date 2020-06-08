rm( list = ls() )

library(tidyverse)
library(fields)
library(lubridate)
library(leaps)

# make sure src is the current working directory

load("../data/phosPrediction.rda")

del <- c("date", "delta_pct", "detla_mg_p_l", "daft_sub_gpm", "daft_sub_gal")

phos.data.clean <- phos.data %>% select(-all_of(del))

set.seed(1)
k <- 10
n <- nrow(phos.data.clean)
num.vars <- length(colnames(phos.data.clean)) - 1

folds <- sample(1:k, n, replace = TRUE)
cv.errors <- matrix(NA, k, num.vars, dimnames = list(NULL, c(1:num.vars)))

for ( j in 1:k ) {
  best.fit <- regsubsets(op_mg_p_l ~ ., data = phos.data.clean[folds != j,], nvmax = num.vars-2)
  testmat <- model.matrix(op_mg_p_l ~ ., data = phos.data.clean[folds == j,])
  for ( i in 1:num.vars ) {
    coefi <- coef(best.fit, id = i)
    xvars <- colnames(coefi)
    pred <- testmat[,xvars] %*% coefi
    cv.errors[j, i] <- mean( (phos.data.clean$op_mg_p_l[folds == j] - pred)^2 , na.rm = TRUE)
  }
}

msep <- apply(cv.errors, 2, mean, na.rm = TRUE)









