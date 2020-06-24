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


X.ind.alum <- full_ancova %>%
  filter(coagulant == "Alum") %>% 
  select(-c(date, coagulant))

X.ind.ferric <- full_ancova %>%
  filter(coagulant == "Ferric") %>% 
  select(-c(date, coagulant))

num.features <- length( names(X.ind.alum) ) - 1

# lasso method
set.seed(1)
k = 10
grid = 10^seq(-2, 8, length=100)
folds = sample(1:k, nrow(X.ind), replace=TRUE)
cv.errors = matrix(NA, k, 100, dimnames=list(NULL, c(1:100)))

x = model.matrix(Salary ~ ., data=X.ind)[,-1]
y = X.ind$Salary
for (j in 1:k) {
  lasso = glmnet(x[folds!=j,], y[folds!=j], alpha=1, lambda=grid)
  testmat = model.matrix(Salary ~ ., data=X.ind[folds==j,])
  for (i in 1:100) {
    coefi = coef(lasso)[,i]
    pred = testmat%*%coefi
    cv.errors[j, i] = mean((X.ind$Salary[folds==j]-pred)^2)
  }
}

msep <- apply(cv.errors, 2, mean)
min.index <- which.min(msep)
lambda.min <- grid[101-min.index]
msep.min <- min(msep)

lasso.full <- glmnet(x, y, alpha=1, lambda=grid)
lasso.coef <- predict(lasso.full, s=lambda.min, type="coefficients")

# extract the sparse matrix elinating the . entries
lass.coef.tibble<- tibble(name = lasso.coef@Dimnames[[1]][lasso.coef@i + 1], coefficient = lasso.coef@x)
lass.coef.tibble$name<- lass.coef.tibble$name %>% str_replace_all("`", "")

X.pred.lasso <- cbind(X.ind$Salary, X.ind %>% select(lass.coef.tibble$name[-1]))
names(X.pred.lasso)[1] <- 'Salary'

fit.lasso <- lm(Salary~., data=X.pred.lasso)
resid.lasso <- fit.lasso$residuals
adjr2.lasso <- summary(fit.lasso)$adj.r.squared
Salary.lasso <- fit.lasso$fitted.values
