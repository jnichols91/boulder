rm( list = ls() )

library(tidyverse)
library(fields)
library(lubridate)
library(leaps)

# make sure src is the current working directory

load("../data/phosPrediction.rda")
del <- c("hour","delta_pct", "detla_mg_p_l", "daft_sub_gpm", "daft_sub_gal", "primary_sludge_gmp_hourly_avg", "twas_flow_gpm_hourly_avg")

phos.data.clean <- phos.data %>% select(-all_of(del))
phos.data.clean <- phos.data.clean[15:107,]
phos.data.clean$centrate_gal[is.na(phos.data.clean$centrate_gal)] <- 0

set.seed(1)
k <- 10
n <- nrow(phos.data.clean)
num.features <- length(names(phos.data.clean)) - 1

folds = sample( 1:k, n, replace=TRUE )
cv.errors = matrix( NA, k, num.features, dimnames = list( NULL,c(1:num.features) ) )

for (j in 1:k){
  best.fit = regsubsets( op_mg_p_l ~., data=phos.data.clean[folds!=j,], nvmax=num.features, method="forward" )
  testmat = model.matrix( op_mg_p_l ~., data = phos.data.clean[folds==j,] )
  for (i in 1:num.features){
    coefi = coef( best.fit, id=i )
    xvars = names( coefi )
    pred = testmat[,xvars]%*%coefi
    cv.errors[j,i] = mean( (phos.data.clean$op_mg_p_l[folds==j]-pred)^2 )
  }
}

msep <- apply(cv.errors, 2, mean)
msep.min <- min(msep)
number.variables <- which.min(msep)

best.fit <- regsubsets( op_mg_p_l ~., data = phos.data.clean, nvmax = number.variables, method = "forward" )
best.kfold <- as.matrix(coef( best.fit,id=number.variables ))
colnames(best.kfold)[1] <- "value"

rownames(best.kfold) <- rownames(best.kfold) %>% str_replace_all("`", "")
data.kfold <- cbind(phos.data.clean$op_mg_p_l, phos.data.clean %>% select(rownames(best.kfold)[-1]))
names(data.kfold)[1] <- 'op_mg_p_l'

fit.kfold <- lm(op_mg_p_l~., data=data.kfold)
resid.kfold <- fit.kfold$residuals
adjr2.kfold <- summary(fit.kfold)$adj.r.squared
op_mg_p_l.kfold <- fit.kfold$fitted.values







