## ----setup, include=FALSE----------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


library(dplyr) 
library(readr)
library(ggplot2)
library(pwr)
library(EnvStats)
library(reshape2)
library(PMCMR)
library(readxl)

# set working directory

# read data
cars <- read_csv("cars.csv")

# factor the variables
cars$make <- factor(cars$make)
cars$fuel_type <- factor(cars$fuel_type, levels = c("gas", "diesel"))
cars$aspiration <- factor(cars$aspiration, levels = c("std", "turbo"))
cars$num_of_doors <- factor(cars$num_of_doors, levels = c("two", "four"))
cars$body_style <- factor(cars$body_style)
cars$drive_wheels <- factor(cars$drive_wheels)
cars$engine_location <- factor(cars$engine_location)
cars$engine_type <- factor(cars$engine_type)
cars$fuel_system <- factor(cars$fuel_system)

# Correlation among numerical variables
print("Correlation Matrix:")
Hmisc::rcorr(as.matrix(cars[,c(8:12,17:23)]))

# histogram of price
hist(cars$price, xlab = "Price", main = "Price Distribution")
hist(log(cars$price), xlab = "Log Price", main = "Ln Price Distribution")

# plot of categorical variables
boxplot(cars$price ~ cars$make, xlab = "Make", ylab = "Price", main = "Cars Make vs Price")
par(mfrow = c(2, 2))
boxplot(cars$price ~ cars$aspiration, xlab = "Aspiration",ylab = "Price", main = "Aspiration vs Price")
boxplot(cars$price ~ cars$fuel_type, xlab = "Fuel type",ylab = "Price", main = "Fuel type vs Price")
boxplot(cars$price ~ cars$body_style, xlab = "Body Style",ylab = "Price", main = "Body style vs Price")
boxplot(cars$price ~ cars$drive_wheels, xlab = "Drive wheel",ylab = "Price", main = "Drive wheel vs Price")


# plot of numerical variables
par(mfrow=c(1,1))
plot(cars$price~cars$horsepower, xlab = "horsepower", ylab = "price", main = "horsepower vs price")
par(mfrow = c(2, 2))
plot(cars$price~cars$wheel_base, xlab = "wheel base", ylab = "price", main = "wheel base vs price")
plot(cars$price~cars$curb_weight, xlab = "curb weight", ylab = "price", main = "curb weight vs price")
plot(cars$price~cars$length, xlab = "length", ylab = "price", main = "length vs price")
plot(cars$price~cars$city_mpg, xlab = "city mpg", ylab = "price", main = "city mpg vs price")
par(mfrow=c(1,1))



#select data for initial linear model
data1 <- cars[ , c("price", "horsepower", "fuel_type", "aspiration", "num_of_doors",
                        "body_style", "drive_wheels", "make", "city_mpg", "length")]

# variables
price <- data1$price
hp <- data1$horsepower
ft <- data1$fuel_type
asp <- data1$aspiration
nod <- data1$num_of_doors
bs <- data1$body_style
dw <- data1$drive_wheels
make <- data1$make
cm <- data1$city_mpg
len <- data1$length

# plot relation between categorical fields and price
pairs(data1[,c(1,3:8)], panel = panel.smooth)

# plot relation between numerical fields and price
pairs(data1[,c(1,2,9,10)], panel = panel.smooth)

# As we can see from the scatter plots, relationship between price and horsepower is almost linear.
# Where as, relationship between price and city_mpg, length is not linear. So we need to apply some transformation to make relationship more linear.
# we will choose to apply log transformation to price.
# to maintain linear relationship between price and hp, we will apply log transformation to horsepower also.

# transforming the variables
# log and inverse transformations
ln_price <- log(price)
in_price <- 1/price
ln_cm <- log(cm)
ln_len <- log(len)
ln_hp <- log(hp)

# plot of the variables after transformation

pairs(matrix(c(ln_price,hp), nrow = 193, ncol = 2, dimnames = list(c(), c("ln_price", "horsepower"))), panel = panel.smooth)

pairs(matrix(c(ln_price,cm), nrow = 193, ncol = 2, dimnames = list(c(), c("ln_price", "city_mpg"))), panel = panel.smooth)

pairs(matrix(c(ln_price,len), nrow = 193, ncol = 2, dimnames = list(c(), c("ln_price", "length"))), panel = panel.smooth)

# the relationship curves are almost linear 
# so we will proceed with the transformed price variable
# also transformation accounts for correcting skewness in the price variable 

# checking for correlation
Hmisc::rcorr(as.matrix(data1[,c(1,2,9,10)])) 
# none of the independent variables seem to be having high correlation

# Initial model
rm <- lm(ln_price ~ hp + ft + asp + nod + bs + dw + make + cm + len)
summary(rm)
residuals_rm <- residuals(rm)

# normality assessment
nortest::ad.test(residuals(rm))
# normality assessment is satisfied for log price

# variable inflation factor
car::vif(rm)


# add log price to main data set
cars$ln_price <- log(cars$price)

# make a subset
cars1 <- cars[,-24]

# full model
fm <- lm(ln_price ~ ., data = cars1)

# partial f-test
anova(rm, fm)

# step wise regression
k <- olsrr::ols_step_both_p(fm, prem = 0.05, pent = 0.05, details = TRUE)

# variables
cw <- cars$curb_weight
height <- cars$height
wb <- cars$wheel_base
el <- cars$engine_location


# final model
final_model <- lm(ln_price ~ cw + make + hp + bs + height + wb + asp + el)
summary(final_model)

residuals_final <- residuals(final_model)
residuals.final_sq <- residuals_final^2
predict_final <- predict(final_model)
predict_final_sq <- predict_final^2

# Multi Collinearity
print("Checking for Multi-Collinearity")
car::vif(final_model)

# Normality Assessment
hist(residuals_final, main = "Normality Assessment : Plot of Residuals") ## informal
nortest::ad.test(residuals(final_model)) ## formal

# Constant Variance Assessment
plot(predict_final, residuals_final, xlab = "Predict Y", ylab = "Residuals", main = "Constant Variance Assessment : Residuals vs Predict Y")

# Independence Assessment
### Graphical - Residuals(t) versus Residuals(t-1)
lag.plot(residuals_final, lags = 1, do.lines = FALSE, 
         diag = FALSE, 
         main = "Independence Assessment : Residuals versus Lag 1 Residuals")
abline(h=0, v=0)


# get p-values
p_value_table <- as.data.frame(summary(final_model)$coefficients[,4])
p_value_table$variables <- rownames(p_value_table)
rownames(p_value_table) <- NULL
colnames(p_value_table) <- c('p_value', 'variables')

# rank p-values
p_value_table <- p_value_table %>% mutate(rank_p = rank(p_value, ties.method = 'min'))

# BH metric
p_value_table$bh <- 0.1 * p_value_table$rank_p / nrow(p_value_table)

# Checking if p-value is less than BH metric
p_value_table$label <- ifelse(p_value_table$p_value <= p_value_table$bh, TRUE, FALSE)
p_value_table[,c(2,1,3,5)] %>% arrange(rank_p)

# Taking the subset of p-values
subset_p <- p_value_table[p_value_table$label == TRUE,]

## FDR function - extract p-value cutoff for E[fdf] < q
fdr <- function(pvals, q, plotit=FALSE){
  pvals <- pvals[!is.na(pvals)]
  N <- length(pvals)
  
  k <- rank(pvals, ties.method="min")
  alpha <- max(pvals[ pvals <= (q*k/N) ])
  
  if(plotit){
    sig <- factor(pvals <= alpha)
    o <- order(pvals)
    plot(pvals[o], log="xy", col=c("grey60","red")[sig[o]], pch=20, 
      ylab="p-values", xlab="tests ordered by p-value", main = paste('FDR =',q))
    lines(1:N, q*(1:N) / N)
  }
  
  return(alpha)
}

# BHvalue for cutoff and plot
fdr(p_value_table$p_value, 0.1, plotit = TRUE)

