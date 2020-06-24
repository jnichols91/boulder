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


X.ind.alum <- lasso_data %>%
  filter(coagulant == "Alum") %>% 
  select(-c(date, coagulant))

X.ind.ferric <- lasso_data %>%
  filter(coagulant == "Ferric") %>% 
  select(-c(date, coagulant))

num.features <- length( names(X.ind.alum) ) - 1

# lasso method
set.seed(1)
k = 10
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

lasso.full <- glmnet(x, y, alpha=1, lambda=grid)
lasso.coef <- predict(lasso.full, s=lambda.min, type="coefficients")

# extract the sparse matrix elinating the . entries
lass.coef.tibble<- tibble(name = lasso.coef@Dimnames[[1]][lasso.coef@i + 1], coefficient = lasso.coef@x)
lass.coef.tibble$name<- lass.coef.tibble$name %>% str_replace_all("`", "")

X.pred.lasso <- cbind(X.ind.alum$op_mg_p_l, X.ind.alum %>% select(lass.coef.tibble$name[-1]))
names(X.pred.lasso)[1] <- 'op_mg_p_l'

fit.lasso <- lm(op_mg_p_l~., data=X.pred.lasso)
resid.lasso <- fit.lasso$residuals
adjr2.lasso <- summary(fit.lasso)$adj.r.squared
OP.lasso <- fit.lasso$fitted.values

# plot of residuals vs. fitted 
pdf(file="../plots/lasso/resid_lasso.pdf", bg="transparent", width=6, height=4.8)
plot(OP.lasso, resid.lasso, pch=20, cex=0.75,
     xlab=TeX("Fitted Values"), ylab = TeX("Residuals"))
text(-0.7,-1.5, TeX(sprintf("Adj. $R^2 = %.3f", adjr2.lasso)), cex=0.85)
abline( h = 0, col = 'red', lwd = 1, lty=2 )
dev.off()

# plot of actual vs. fitted
pdf(file="../plots/lasso/actVSfit_lasso.pdf", bg="transparent", width=6, height=4.8)
plot(OP.lasso, X.ind.alum$op_mg_p_l, pch=20, cex=0.75,
     xlab=TeX("Fitted Values"), ylab = TeX("Actual Values"))
text(-0.5,2.5, TeX(sprintf("Adj. $R^2 = %.3f", adjr2.lasso)), cex=0.85)
abline( a=0,b=1, col = 'red', lwd = 1, lty=2 )
dev.off()

#Q-Q plot
pdf(file="../plots/lasso/qqplot_lasso.pdf", bg="transparent", width=6, height=4.8)
qqnorm(resid.lasso, xlab=TeX("Theoretical Quantiles"), ylab=TeX("Standardized Residuals"), main="")
qqline(resid.lasso, lty=3, col='seashell4')
dev.off()
