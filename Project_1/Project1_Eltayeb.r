# Project 1

#### 1 a) #### 


load( file = "/home/neko/RWTH/Master/Erasmus/Vorlesungen/LinLog/R/weather.rda")
#load("~/Desktop/LinLog/Project_1/Data/weather.rda")

summary(weather)
head(weather)

x <- weather$temp # Temperature
Y <- weather$rain # Rain
plot(x, Y, main = "Rain vs Temperature", xlab = "Temperature", ylab = "Rain")

# Does the relationship look linear? - No.

# Fitting a linear regression model
(model <- lm(Y ~ x, data = weather)) 
model_summary <- summary(model)

# Betas (B0 = 37.500 , B1 = 1.301)
beta_estimates <- model$coefficients
B0 <- beta_estimates[1]
B1 <- beta_estimates[2]

# Estimation of standard deviation
se_B0 <- model_summary$coefficients[1,2]
se_B1 <- model_summary$coefficients[2,2]

# 95 % confidence intervals
confInt <- confint(model)

average_start <- B0 + B1*0 # Average at the beginning = 37.500 + 1.3011 (mm/C)
# => increase by 30%

library(ggplot2)

(
  plot_data <- ggplot(data = weather, 
                      aes(x = x, y = Y)) + 
    geom_point(size = 1) +
    xlab("Temperature") +
    ylab("Rain")+
    theme(text = element_text(size = 16))
)

# Add the fitted line to the data plot
# Without confidence interval: se = FALSE:
(
  plot_data + 
    geom_smooth(method = lm, se = FALSE) +
    labs(caption = "weather data and fitted line")
)

# Add fitted line WITH confidence interval
(
  plot_conf <- plot_data + 
    geom_smooth(method = lm, fill="red") +
    labs(caption = "Fitted line and 95% confidence interval")
)

# Calculate and add the prediction interval
rain_pred <- 
  cbind(weather, 
        pred = predict(model, interval = "prediction"))
head(weather)

# Add the prediction interval to the confidence interval plot
(
  plot_conf +
    geom_line(data = rain_pred, aes(y = pred.lwr),
              color = "red", linetype = "dashed", size = 1) +
    geom_line(data = rain_pred, aes(y = pred.upr),
              color = "red", linetype = "dashed", size = 1) +
    labs(caption = "95% confidence and prediction intervals for fitted line in weather data")
)

# Does it look reasonable? 
# Not really, large confidence-and prediction-intervals

#### Basic residual analysis, 1 a) ####

# Add the residuals to the predicted data. 
rain_pred$e <- model$residuals
head(rain_pred)

# Save the max-value in order to make the y-axis symmetrical in the plots.
(max.e <- max(abs(rain_pred$e)))
(rain_elims <- c(-max.e, max.e))

# Plot residuals against yhat, add a horizontal line at y=0,
# and expand the y-axis to include +/- max residual.

ggplot(data = rain_pred, 
       aes(x = pred.fit, y = e)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0) +
  expand_limits(y = rain_elims) +
  xlab("Predicted amount of rain") +
  ylab("Residual") +
  labs(tag = "B") +
  labs(title = "Residuals vs predicted values Y-hat") +
  theme(text = element_text(size = 18))

# Make a normal QQ-plot of residuals.
ggplot(data = rain_pred, 
       aes(sample = e)) +
  geom_qq(size = 3) +
  geom_qq_line() +
  theme(text = element_text(size = 18))

# Histogram of the residuals:
ggplot(data = rain_pred,
       aes(x = e)) +
  geom_histogram(bins = 20) +
  xlab("Residuals") +
  theme(text = element_text(size = 18))

# Not a good model because it does not look gaussian.


#### 1 b) #### 

rm(list=ls())

load( file = "/home/neko/RWTH/Master/Erasmus/Vorlesungen/LinLog/R/weather.rda")
#load("~/Desktop/LinLog/Project_1/Data/weather.rda")

x <- weather$temp # Temperature
Y <- log(weather$rain) # Rain, but now log-transformed because it looked like an exp-increase.

plot(x, Y, main = "Rain vs Temperature", xlab = "Temperature", ylab = "Rain")

# Fitting a linear regression model
(model <- lm(Y ~ x, data = weather)) 
model_summary <- summary(model)

# Betas (B0 = 5.78940 , B1 = 0.09744)
beta_estimates <- model$coefficients
B0 <- beta_estimates[1]
B1 <- beta_estimates[2]

# Estimation of standard deviation
se_B0 <- model_summary$coefficients[1,2]
se_B1 <- model_summary$coefficients[2,2]

# 95 % confidence intervals
confInt <- confint(model)

average_start <- B0 + B1*0 # Average at the beginning =  (mm/C)

library(ggplot2)

(
  plot_data <- ggplot(data = weather, 
                      aes(x = x, y = Y)) + 
    geom_point(size = 1) +
    xlab("Temperature") +
    ylab("Rain") +
    labs(title = "Rain (mm)/ Temperature(C)") +
    theme(text = element_text(size = 16))
)

# Add the fitted line to the data plot
# Without confidence interval: se = FALSE:
(
  plot_data + 
    geom_smooth(method = lm, se = FALSE) +
    labs(caption = "weather data and fitted line")
)

# Add fitted line WITH confidence interval
(
  plot_conf <- plot_data + 
    geom_smooth(method = lm, fill="red") +
    labs(caption = "Fitted line and 95% confidence interval")
)

# Calculate and add the prediction interval
rain_pred <- 
  cbind(weather, 
        pred = predict(model, interval = "prediction"))
head(weather)

# Add the prediction interval to the confidence interval plot
(
  plot_conf +
    geom_line(data = rain_pred, aes(y = pred.lwr),
              color = "red", linetype = "dashed", size = 1) +
    geom_line(data = rain_pred, aes(y = pred.upr),
              color = "red", linetype = "dashed", size = 1)
)

# Does it look reasonable? 
# Not really, large confidence-and prediction-intervals

#### Basic residual analysis, 1 b) ####

# Add the residuals to the predicted data. 
rain_pred$e <- model$residuals
head(rain_pred)

# Save the max-value in order to make the y-axis symmetrical in the plots.
(max.e <- max(abs(rain_pred$e)))
(rain_elims <- c(-max.e, max.e))

# Plot residuals against yhat, add a horizontal line at y=0,
# and expand the y-axis to include +/- max residual.

ggplot(data = rain_pred, 
       aes(x = pred.fit, y = e)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0) +
  expand_limits(y = rain_elims) +
  xlab("Predicted amount of rain") +
  ylab("Residual") +
  labs(tag = "B") +
  labs(title = "Residuals vs predicted values Y-hat") +
  theme(text = element_text(size = 18))

# Make a normal QQ-plot of residuals.
ggplot(data = rain_pred, 
       aes(sample = e)) +
  geom_qq(size = 3) +
  geom_qq_line() +
  theme(text = element_text(size = 18))

# Histogram of the residuals:
ggplot(data = rain_pred,
       aes(x = e)) +
  geom_histogram(bins = 20) +
  xlab("Residuals") +
  theme(text = element_text(size = 18))

## Conclusion for 1 b): Better but not as good as we would want.


#### 1 c) #### 

# Discussion part, use histograms, QQ-plot to argue why it was good/bad!
# Good link for transformations: https://rcompanion.org/handbook/I_12.html
# Tried sqrt/log/cubic-transformations. The best one is when the residual looks normal distributed.

#### 1 d) #### 

# Level of rain over temp + pressure
# y = B0 + B1 * x + epsilon <-> log(Y) = B0 + B1 * x + epsilon
# Transformation -> Y = exp(B0) * exp(B1)^x * exp(epsilon) = a * b^x * exp(epsilon),
# where a = exp(B0) and b = exp(B1)

# Estimates of a and b
#beta_estimates <- model.mult_sum$coefficients

a <- exp(beta_estimates[1]) # a = 1.041
b <- exp(beta_estimates[2]) # b = 0.943

a*b^1

#### 1 e) #### 
# See above plots.

#### 1 f) #### 

# Get confidence and prediction interval for x0 = 5 degrees celsius
mm_x0 <- data.frame(x = c(5))

(rain_pred_y0 <- cbind(mm_x0,
                     pred = predict(model, mm_x0,
                                    interval = "prediction")))

#### Section 2 ####

rm(list=ls())

load( file = "RWTH/Master/Erasmus/Vorlesungen/LinLog/R/weather.rda")
#load("~/Desktop/LinLog/Project_1/Data/weather.rda")

#### 2 a) ####

# summary(model), look at p-value. Should be smaller than 0.05.


#### 2 b) ####

## Plotting temp vs rain, temp vs pressure, rain vs pressure

# create three plots
# with the log, it looks better, but still not perfect
library(ggplot2)

# temp vs rain
ggplot(data = weather, 
       aes(x = temp, y = rain)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0) +
  xlab("Temperature") +
  ylab("Rain") +
  theme(text = element_text(size = 18))

# temp vs pressure
ggplot(data = weather, 
       aes(x = temp, y = pressure)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0) +
  ylim(980,1040) +
  xlab("Temperature") +
  ylab("Pressure")
  theme(text = element_text(size = 18))

# pressure vs rain
ggplot(data = weather, 
       aes(x = pressure, y = log(rain))) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0) +
  xlab("Pressure") +
  ylab("Rain")
  theme(text = element_text(size = 18))


#### 2 c) ####

# Fitting a linear regression model
(model.mult <- lm(log(rain) ~ temp + pressure, data = weather)) 
model.mult_sum <- summary(model.mult)

# Betas (B0 =  , B1 = )
beta_estimates <- model.mult_sum$coefficients
B0 <- beta_estimates[0]
B1 <- beta_estimates[1]
B2 <- beta_estimates[2]

# Estimation of standard deviation
se_B0 <- model.mult_sum[["coefficients"]][0,2]
se_B1 <- model.mult_sum$coefficients[1,2]
se_B2 <- model.mult_sum$coefficients[2,2]

# 95 % confidence intervals
confInt <- confint(model.mult)

average_start <- B0 + B1*0 # Average at the beginning = 37.500 + 1.3 (mm/C)

confInt # All the Betas are signifcantly different from 0 and are therefore needed.


#### 2 d) ####

rain_pred <- 
  cbind(weather, 
        pred = predict(model.mult, interval = "prediction"))

#### Basic residual analysis, 2 d) #### 

# Add the residuals to the predicted data. 
rain_pred$e <- model.mult$residuals
head(rain_pred)

# Save the max-value in order to make the y-axis symmetrical in the plots.
(max.e <- max(abs(rain_pred$e)))
(rain_elims <- c(-max.e, max.e))

# Plot residuals against yhat, add a horizontal line at y=0,
# and expand the y-axis to include +/- max residual.

ggplot(data = rain_pred, 
       aes(x = pred.fit, y = e)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0) +
  expand_limits(y = rain_elims) +
  xlab("Predicted amount of rain") +
  ylab("Residual") +
  theme(text = element_text(size = 18))

## Plot resid vs pressure
ggplot(data = weather, 
       aes(x = pressure, y = rain_pred$e)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0) +
  expand_limits(y = rain_elims) +
  xlab("Pressure") +
  ylab("Residual") +
  theme(text = element_text(size = 18))

## Temp vs resid
ggplot(data = weather, 
       aes(x = temp, y = rain_pred$e)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0) +
  expand_limits(y = rain_elims) +
  xlab("Temperature") +
  ylab("Residual") +
  theme(text = element_text(size = 18))


# Make a normal QQ-plot of residuals.
ggplot(data = rain_pred, 
       aes(sample = e)) +
  geom_qq(size = 3) +
  geom_qq_line() +
  theme(text = element_text(size = 18))

# Histogram of the residuals:
ggplot(data = rain_pred,
       aes(x = e)) +
  geom_histogram(bins = 20) +
  xlab("Residuals") +
  labs(title = "Histogram of residuals") +
  theme(text = element_text(size = 18))

### Plots that were asked for
# 
# # Pressure vs Precipitation
# ggplot(data = weather, 
#        aes(x = pressure, y = rain)) +
#   geom_point(size = 3) +
#   expand_limits(y = rain_elims) +
#   xlab("Pressure") +
#   ylab("Precipitation") +
#   labs(title = "Pressure vs Precipitation") +
#   theme(text = element_text(size = 18))
# 
# # Pressure vs Temperature
# ggplot(data = weather, 
#        aes(x = pressure, y = temp)) +
#   geom_point(size = 3) +
#   expand_limits(y = rain_elims) +
#   xlab("Pressure") +
#   ylab("Temperature") +
#   labs(title = "Pressure vs Temperature") +
#   theme(text = element_text(size = 18))
# 
# ##

#### 2 e) ####


# Level of rain over temp + pressure
# y = B0 + B1 * x + epsilon <-> log(Y) = B0 + B1 * x + epsilon
# Transformation -> Y = exp(B0) * exp(B1)^x * exp(epsilon) = a * b^x * exp(epsilon),
# where a = exp(B0) and b = exp(B1)

# Estimates of a and b
beta_estimates <- model.mult_sum$coefficients
a <- exp(beta_estimates[1]) # Intercept
a2 <- exp(beta_estimates[2]) # Temp in this case
a3 <- exp(beta_estimates[3]) # Pressure in this case

# 95% confidence intervals
 log_confInt <- exp(confInt)
# nope. This is wrong. We want the prediction interval.
rain_pred

# Average precipitation change by increasing with 1 degree.
# change does not care about 'a' since 'a' is a constant. 
 a2^1 
# 1.04 => increase by 4% 
#### 2 f) ####
a3^20
# 0.3142831 => decrease by 69%

#### 2 g)  ####

w.x0 = data.frame(temp = c(5,5) , pressure = c(1000, 1020))
cbind(w.x0, exp(predict(model.mult, w.x0, interval = "prediction")))



#### 2.3 Temperature and pressure with interaction #####

rm(list=ls())

#load( file = "RWTH/Master/Erasmus/Vorlesungen/LinLog/R/weather.rda")
load("~/Desktop/LinLog/Project_1/Data/weather.rda")

#### 2 h) ####

# Fitting a linear regression model
(model.mult <- lm(log(rain) ~ temp * pressure, data = weather)) 
sum_mult <- summary(model.mult)

beta_estimates <- sum_mult$coefficients

confint(model.mult)

#### 2 i) ####
# Ask Anna, what is that is given by the hint that would answer the question?
# we compare model.mult1 and model.mult2. The parameters change drastically.

(model.mult1 <- lm(log(rain) ~ temp * pressure, data = weather))

(model.mult2 <- lm(log(rain) ~ temp * I(pressure - 1012), data = weather)) 
(model.plus <- lm(log(rain) ~ temp + I(pressure - 1012), data = weather)) 

beta_estimates <- sum_mult$coefficients

confint(model.mult)
confint(model.plus)

summary(model.mult)

#### 2 j) ####

rain_pred <- 
  cbind(weather, 
        pred = predict(model.mult, interval = "prediction"))

#### Basic residual analysis, 2 j) ####

# Why does it feel like something is missing?
# We are not. I showed Anna our plots and she said it doesn't get better much. So no worries.

# Add the residuals to the predicted data. 
rain_pred$e <- model.mult$residuals
head(rain_pred)

# Save the max-value in order to make the y-axis symmetrical in the plots.
(max.e <- max(abs(rain_pred$e)))
(rain_elims <- c(-max.e, max.e))

# Plot residuals against yhat, add a horizontal line at y=0,
# and expand the y-axis to include +/- max residual.

ggplot(data = rain_pred, 
       aes(x = pred.fit, y = e)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0) +
  expand_limits(y = rain_elims) +
  xlab("Predicted amount of rain") +
  ylab("Residual") +
  theme(text = element_text(size = 18))

# Make a normal QQ-plot of residuals.
ggplot(data = rain_pred, 
       aes(sample = e)) +
  geom_qq(size = 3) +
  geom_qq_line() +
  theme(text = element_text(size = 18))

# Histogram of the residuals:
ggplot(data = rain_pred,
       aes(x = e)) +
  geom_histogram(bins = 20) +
  xlab("Residuals") +
  theme(text = element_text(size = 18))

#### 2 k) #### 
w.x0 = data.frame(temp = c(1,1) , pressure = c(1000, 1020))
cbind(w.x0, exp(predict(model.mult, w.x0, interval = "prediction")))

#### 2 l) #### 
w.x1 = data.frame(temp = c(-10,10) , pressure = c(1000, 1020))
cbind(w.x1, exp(predict(model.mult, w.x1, interval = "prediction")))

#### 2.4 Temperature, pressure and location #### 

####  2 m) + n) #### 
rm(list=ls())

#load( file = "/home/neko/RWTH/Master/Erasmus/Vorlesungen/LinLog/R/weather.rda")
load("~/Desktop/LinLog/Project_1/Data/weather.rda")

summary(weather)
# Fitting a linear regression model, with Uppsala as reference location because it got the most observations.
weather$location <- relevel(weather$location, "Uppsala")

(model.mult_loc <- lm(log(rain) ~ temp * pressure + location, data = weather)) 
(testmodel <- lm(log(rain) ~ temp * pressure, data = weather)) 
sum_mult_loc <- summary(model.mult_loc)
sum_testmodel <- summary(testmodel)

beta_estimates <- sum_mult_loc$coefficients

confint(model.mult_loc)
confint(testmodel)
# Test of our choice.
anova(model.mult_loc, testmodel)

#### 2 o) ####
# we just look at the values and compare them. no need for fancy tests. 

#### Basic residual analysis, 2 p) ####

library(ggplot2)

pred_multiloc <- 
  cbind(weather,
        conf = predict(model.mult_loc, interval = "confidence"),
        pred = predict(model.mult_loc, interval = "prediction"),
        e = residuals(model.mult_loc))
head(pred_multiloc)

elim <- max(abs(pred_multiloc$e)) * c(-1, 1)
ggplot(pred_multiloc, aes(x = fit, y = e, color = location)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  expand_limits(y = elim) +
  facet_wrap(~ location)

ggplot(pred_multiloc, aes(x =  temp, y = e, color = location)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  expand_limits(y = elim) +
  facet_wrap(~ location)

ggplot(pred_multiloc, aes(x =  pressure, y = e, color = location)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  expand_limits(y = elim) +
  facet_wrap(~ location)

ggplot(pred_multiloc, aes(sample = e)) +
  geom_qq() + geom_qq_line() +
  expand_limits(y = elim)

ggplot(pred_multiloc, aes(sample = e, color = location)) +
  geom_qq() + geom_qq_line() +
  expand_limits(y = elim)+
  facet_wrap(~ location)


# Save the max-value in order to make the y-axis symmetrical in the plots.
(max.e <- max(abs(pred_multiloc$e)))
(rain_elims <- c(-max.e, max.e))

# Plot residuals against yhat, add a horizontal line at y=0,
# and expand the y-axis to include +/- max residual.
ggplot(data = pred_multiloc, 
       aes(x = pred.fit, y = e)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0) +
  expand_limits(y = rain_elims) +
  xlab("Predicted amount of rain") +
  ylab("Residual") +
  theme(text = element_text(size = 18))

# Make a normal QQ-plot of residuals.
ggplot(data = pred_multiloc, 
       aes(sample = e)) +
  geom_qq(size = 3) +
  geom_qq_line() +
  theme(text = element_text(size = 18))

# Histogram of the residuals:
ggplot(data = pred_multiloc,
       aes(x = e)) +
  geom_histogram(bins = 20) +
  xlab("Residuals") +
  theme(text = element_text(size = 18))

#### 2 q) ####

weather$location <- relevel(weather$location, "Uppsala")
ggplot(weather, aes(x = I(pressure - 1012)*temp, y = rain, color = location)) +
  geom_point() +
  facet_wrap(~ location)
ggplot(weather, aes(x = I(pressure - 1012)*temp, y = log(rain), color = location)) +
  geom_point() +
  facet_wrap(~ location)

# Based on one value, Lund (southern Sweden likes rain....)
# Uppsala is a close contender to being shit as well, but Skane ftw.


#### 3 Precipitation — which variables are needed? ####

#### 3.1 Outliers and influential observations ####


#### Section 3 ####
#### 3 a) ####
model.2n <- model.mult_loc
(pplus1.2n <- length(model.2n$coefficients))
# n = number of observations
(n <- nrow(weather))

# save data, fitted values and leverage:
w.diagnostics <-
  cbind(weather,
        fit = predict(model.2n),
        v = influence(model.2n)$hat)
head(w.diagnostics)

ggplot(w.diagnostics, aes(x = fit, y = v)) +
  geom_jitter(width = 1) +
  expand_limits(y = 0) +
  geom_hline(yintercept = 1/n) +
  geom_hline(yintercept = 0.026, color = "blue") +
  geom_hline(yintercept = 2*pplus1.2n/n, 
             color = "red", linetype = "dashed", size = 1) +
  facet_wrap(~ location)
  
ggplot(w.diagnostics, aes(x = pressure, y = v)) +
  geom_jitter(width = 1) +
  expand_limits(y = 0) +
  geom_hline(yintercept = 1/n) +
  geom_hline(yintercept = 0.026, color = "blue") +
  geom_hline(yintercept = 2*pplus1.2n/n, 
             color = "red", linetype = "dashed", size = 1) +
  facet_wrap(~ location)

ggplot(w.diagnostics, aes(x = temp, y = v)) +
  geom_jitter(width = 1) +
  expand_limits(y = 0) +
  geom_hline(yintercept = 1/n) +
  geom_hline(yintercept = 0.026, color = "blue") +
  geom_hline(yintercept = 2*pplus1.2n/n, 
             color = "red", linetype = "dashed", size = 1) +
  facet_wrap(~ location)
# Uppsala has the most observations. For small data sets, individual points have a higher influence, so Lund und Abisko have higher leverages. 

#### 3 b) ####
I_highleverages <- which(w.diagnostics$v > 0.026)

ggplot(weather, aes(temp, pressure)) +
geom_point() +
geom_point(data = weather[I_highleverages, ], color = "red",
shape = 24, size = 3) +
facet_wrap(~ location)

# Abisko is a lot more north, so the pressure vs. temp model maybe doesn't capture that behaviour

#### 3 c) ####

# Add studentized residuals
tmp.pred <- cbind(weather, 
                  fit = predict(model.2n),
                  e = residuals(model.2n))

tmp.pred$r <- rstudent(model.2n)
head(tmp.pred)

ggplot(tmp.pred, aes(x = fit, y = r)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-2, 2), color = "red") +
  geom_hline(yintercept = c(-4, 4), color = "red", linetype = "dashed") +
  geom_point(data = tmp.pred[I_highleverages, ], color = "red",
             shape = 24, size = 3) +
  facet_wrap(~ location) +
  xlab("Fitted values") +
  ylab("r*") +
  # labs(title = "Weather: studentized residuals vs fitted values") +
  labs(caption = "y = +/- 2 and +/- 4") +
  theme(text = element_text(size = 18))
# Conclusion: No problematic studentized residuals for Abisko,
# but 1 for Uppsala and maybe 1 for Lund.

#### 3 d) ####

I_SR <- which(abs(tmp.pred$r) > 4)

I_HL_SR <- c(I_highleverages, I_SR)

# Plot of the "outliers" in terms of leverages
ggplot(weather, aes(log(rain), pressure)) +
  geom_point() +
  geom_point(data = weather[I_HL_SR, ], color = "red",
             shape = 24, size = 3) +
  facet_wrap(~ location)

ggplot(weather, aes(log(rain), temp)) +
  geom_point() +
  geom_point(data = weather[I_HL_SR, ], color = "red",
             shape = 24, size = 3) +
  facet_wrap(~ location)

# High pressure with small(er) amounts of rain is causing the problem with the residuals.

#### 3 e) ####
w.diagnostics$D <- cooks.distance(model.2n)
head(w.diagnostics)

ggplot(w.diagnostics, aes(x = pressure, y = D)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = 4/n, color = "red",
             linetype = "dotted", size = 1) +
  facet_wrap(~ location) +
  geom_point(data = w.diagnostics[I_HL_SR, ], 
             color = "red")
#### 3 f) ####

I_cook <- which(w.diagnostics$D > 4/n)

I_total <- c(I_HL_SR, I_cook)

#I_total <- c(which(I_HL_SR_D > 4/n))

Exweather <- weather[-I_total, ]

Exweather$location <- relevel(Exweather$location, "Uppsala")

(modelX <- lm(log(rain) ~ temp * pressure + location, data = Exweather)) 

sum_modelX <- summary(modelX)

beta_estimates <- sum_modelX$coefficients

confint(modelX)
sum_modelX

# Studentized residulas for trimmed dataset

# Add studentized residuals
modelX.pred <- cbind(Exweather, 
                  fit = predict(modelX),
                  e = residuals(modelX))

modelX.pred$r <- rstudent(modelX)
head(modelX.pred)

ggplot(modelX.pred, aes(x = fit, y = r)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-2, 2), color = "red") +
  geom_hline(yintercept = c(-4, 4), color = "red", linetype = "dashed") +
  facet_wrap(~ location) +
  xlab("Fitted values") +
  ylab("r*") +
  # labs(title = "Weather: studentized residuals vs fitted values") +
  labs(caption = "y = +/- 2 and +/- 4") +
  theme(text = element_text(size = 18))
# Conclusion: 

w2.diagnostics <-
  cbind(Exweather,
        fit = predict(modelX),
        v = influence(modelX)$hat)

w2.diagnostics$D <- cooks.distance(modelX)
head(w2.diagnostics)

ggplot(w2.diagnostics, aes(x = pressure, y = D)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = 4/n, color = "red",
             linetype = "dotted", size = 1) +
  facet_wrap(~ location) #+ geom_point(data = w2.diagnostics[I_HL_SR, ],  color = "red")

#### 3.2 Model comparisons ####

#### 3 g) ####

# Model from 1 b)
(modelsec3_1b <- lm(rain ~ temp, data = Exweather))
sum.modelsec3_1b <- summary(modelsec3_1b)

# Model from 2 c)
(modelsec3_2c <- lm(log(rain) ~ temp + pressure, data = Exweather)) 
sum.modelsec3_2c <- summary(modelsec3_2c)


# Model from 2 h)
(modelsec3_2h <- lm(log(rain) ~ temp * pressure, data = Exweather)) 
sum.modelsec3_2h <- summary(modelsec3_2h)


#### R^2 and R^2_adj ####
(collect.R2s <- data.frame(
  nr = seq(1, 4),
  model = c("rain vs temp", "log(rain) vs temp+pressure",  
            "log(rain) vs temp*pressure", "log(rain) vs temp*pressure+location"),
  R2 = c(sum.modelsec3_1b$r.squared,
         sum.modelsec3_2c$r.squared,
         sum.modelsec3_2h$r.squared,
         sum_modelX$r.squared),
  R2.adj = c(sum.modelsec3_1b$adj.r.squared,
             sum.modelsec3_2c$adj.r.squared,
             sum.modelsec3_2h$adj.r.squared,
             sum_modelX$adj.r.squared)))


#### AIC and BIC ####
(collect.AIC <- data.frame(
  nr = seq(1, 4),
  model = c("rain vs temp", "log(rain) vs temp+pressure",  
            "log(rain) vs temp*pressure", "log(rain) vs temp*pressure+location"),
  AIC( modelsec3_1b, modelsec3_2c, modelsec3_2h, modelX),
  BIC( modelsec3_1b, modelsec3_2c, modelsec3_2h, modelX)))


#### 3 h) ####

(modelX_2 <- lm(log(rain) ~ temp * pressure * location, data = Exweather)) 

sum_modelX_2 <- summary(modelX_2)

beta_estimates <- sum_modelX_2$coefficients

confint(modelX_2)
sum_modelX_2

anova(modelX_2,modelX)

AIC(modelX_2,modelX)
BIC(modelX_2,modelX)

# Conclusion: Not suitable to substitute temp * pressure + location
# with temp * pressure * location.

#### 3 i) ####
# How do you remove variables, one by one without refitting the entire model?
step(modelX_2,k = log(nrow(Exweather)))


#### 3 j) ####
model0 <- lm(log(rain) ~ 1, data = Exweather)
step(model0, direction = "forward", k= log(nrow(Exweather)))
step(modelX_2, direction = "forward", k= log(nrow(Exweather)))
# TODO which one?

#### 3 k) ####
# Adding another categorical variable, season.

Exweather$season <- "summer" 
Exweather$season[Exweather$monthnr == 3 ] <- "spring" 
Exweather$season[Exweather$monthnr == 4 ] <- "spring" 
Exweather$season[Exweather$monthnr == 5 ] <- "spring" 
Exweather$season[Exweather$monthnr == 12 ] <- "winter"
Exweather$season[Exweather$monthnr == 1 ] <- "winter"
Exweather$season[Exweather$monthnr == 2 ] <- "winter"
Exweather$season[Exweather$monthnr == 9 ] <- "autumn"
Exweather$season[Exweather$monthnr == 10 ] <- "autumn"
Exweather$season[Exweather$monthnr == 11 ] <- "autumn"


# refit with season and redo step()
