#### Clear environment and load libraries ####
rm( list = ls() )

library(tidyverse)
library(lubridate)
library(leaps)
library(glmnet)
library(latex2exp)


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

num.features <- length( names(X.ind.alum) ) - 1

# lasso method
set.seed(1)
k = 4
grid = 10^seq(-2, 8, length=100)
folds = sample(1:k, nrow(X.ind.alum), replace=TRUE)
cv.errors = matrix(NA, k, 100, dimnames=list(NULL, c(1:100)))

x = model.matrix(op_mg_p_l ~ ., data=X.ind.alum)[,-1]
y = X.ind.alum$op_mg_p_l
for (j in 1:k) {
  lasso = glmnet(x[folds!=j,], y[folds!=j], alpha=1, lambda=grid)
  testmat = model.matrix(op_mg_p_l ~ ., data=X.ind.alum[folds==j,])
  for (i in 1:100) {
    coefi = coef(lasso)[,i]
    pred = testmat%*%coefi
    cv.errors[j, i] = mean((X.ind.alum$op_mg_p_l[folds==j]-pred)^2)
  }
}

msep <- apply(cv.errors, 2, mean)
min.index <- which.min(msep)
lambda.min <- grid[101-min.index]
msep.min <- min(msep)

lasso.full.alum <- glmnet(x, y, alpha=1, lambda=grid)
lasso.coef.alum <- predict(lasso.full.alum, s=lambda.min, type="coefficients")

# extract the sparse matrix elinating the . entries
lass.coef.tibble.alum<- tibble(name = lasso.coef.alum@Dimnames[[1]][lasso.coef.alum@i + 1], coefficient = lasso.coef.alum@x)
lass.coef.tibble.alum$name<- lass.coef.tibble.alum$name %>% str_replace_all("`", "")

X.pred.lasso <- cbind(X.ind.alum$op_mg_p_l, X.ind.alum %>% select(lass.coef.tibble.alum$name[-1]))
names(X.pred.lasso)[1] <- 'op_mg_p_l'

fit.lasso.alum <- lm(op_mg_p_l~., data=X.pred.lasso)
resid.lasso.alum <- fit.lasso.alum$residuals
adjr2.lasso.alum <- summary(fit.lasso.alum)$adj.r.squared
OP.lasso.alum <- fit.lasso.alum$fitted.values

# plot of residuals vs. fitted 
pdf(file="../plots/lasso/alum_resid_lasso.pdf", bg="transparent", width=6, height=4.8)
plot(OP.lasso.alum, resid.lasso.alum, pch=20, cex=0.75,
     xlab=TeX("Fitted Values"), ylab = TeX("Residuals"))
text(2,0.3, TeX(sprintf("Adj. $R^2 = %.3f", adjr2.lasso.alum)), cex=0.85)
abline( h = 0, col = 'red', lwd = 1, lty=2 )
dev.off()

# plot of actual vs. fitted
pdf(file="../plots/lasso/alum_actVSfit_lasso.pdf", bg="transparent", width=6, height=4.8)
plot(OP.lasso.alum, X.ind.alum$op_mg_p_l, pch=20, cex=0.75,
     xlab=TeX("Fitted Values"), ylab = TeX("Actual Values"))
text(2,2.5, TeX(sprintf("Adj. $R^2 = %.3f", adjr2.lasso.alum)), cex=0.85)
abline( a=0,b=1, col = 'red', lwd = 1, lty=2 )
dev.off()

#Q-Q plot
pdf(file="../plots/lasso/alum_qqplot_lasso.pdf", bg="transparent", width=6, height=4.8)
qqnorm(resid.lasso.alum, xlab=TeX("Theoretical Quantiles"), ylab=TeX("Standardized Residuals"), main="")
qqline(resid.lasso.alum, lty=3, col='seashell4')
dev.off()




# lasso method
set.seed(1)
k = 3
grid = 10^seq(-2, 8, length=100)
folds = sample(1:k, nrow(X.ind.ferric), replace=TRUE)
cv.errors = matrix(NA, k, 100, dimnames=list(NULL, c(1:100)))

x = model.matrix(op_mg_p_l ~ ., data=X.ind.ferric)[,-1]
y = X.ind.ferric$op_mg_p_l
for (j in 1:k) {
  lasso = glmnet(x[folds!=j,], y[folds!=j], alpha=1, lambda=grid)
  testmat = model.matrix(op_mg_p_l ~ ., data=X.ind.ferric[folds==j,])
  for (i in 1:100) {
    coefi = coef(lasso)[,i]
    pred = testmat%*%coefi
    cv.errors[j, i] = mean((X.ind.ferric$op_mg_p_l[folds==j]-pred)^2)
  }
}

msep <- apply(cv.errors, 2, mean)
min.index <- which.min(msep)
lambda.min <- grid[101-min.index]
msep.min <- min(msep)

lasso.full.ferric <- glmnet(x, y, alpha=1, lambda=grid)
lasso.coef.ferric <- predict(lasso.full.ferric, s=lambda.min, type="coefficients")

# extract the sparse matrix elinating the . entries
lass.coef.tibble.ferric<- tibble(name = lasso.coef.ferric@Dimnames[[1]][lasso.coef.ferric@i + 1], coefficient = lasso.coef.ferric@x)
lass.coef.tibble.ferric$name<- lass.coef.tibble.ferric$name %>% str_replace_all("`", "")

X.pred.lasso <- cbind(X.ind.ferric$op_mg_p_l, X.ind.ferric %>% select(lass.coef.tibble.ferric$name[-1]))
names(X.pred.lasso)[1] <- 'op_mg_p_l'

fit.lasso.ferric <- lm(op_mg_p_l~., data=X.pred.lasso)
resid.lasso.ferric <- fit.lasso.ferric$residuals
adjr2.lasso.ferric <- summary(fit.lasso.ferric)$adj.r.squared
OP.lasso.ferric <- fit.lasso.ferric$fitted.values

# plot of residuals vs. fitted 
pdf(file="../plots/lasso/ferr_resid_lasso.pdf", bg="transparent", width=6, height=4.8)
plot(OP.lasso.ferric, resid.lasso.ferric, pch=20, cex=0.75,
     xlab=TeX("Fitted Values"), ylab = TeX("Residuals"))
text(3.2,-0.4, TeX(sprintf("Adj. $R^2 = %.3f", adjr2.lasso.ferric)), cex=0.85)
abline( h = 0, col = 'red', lwd = 1, lty=2 )
dev.off()

# plot of actual vs. fitted
pdf(file="../plots/lasso/ferr_actVSfit_lasso.pdf", bg="transparent", width=6, height=4.8)
plot(OP.lasso.ferric, X.ind.ferric$op_mg_p_l, pch=20, cex=0.75,
     xlab=TeX("Fitted Values"), ylab = TeX("Actual Values"))
text(3.2,2, TeX(sprintf("Adj. $R^2 = %.3f", adjr2.lasso.ferric)), cex=0.85)
abline( a=0,b=1, col = 'red', lwd = 1, lty=2 )
dev.off()

#Q-Q plot
pdf(file="../plots/lasso/ferr_qqplot_lasso.pdf", bg="transparent", width=6, height=4.8)
qqnorm(resid.lasso.ferric, xlab=TeX("Theoretical Quantiles"), ylab=TeX("Standardized Residuals"), main="")
qqline(resid.lasso.ferric, lty=3, col='seashell4')
dev.off()



