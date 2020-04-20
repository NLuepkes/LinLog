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
  labs(tag = "C") +
  labs(title = "Normal QQ-plot of residuals") +
  theme(text = element_text(size = 18))

# Histogram of the residuals:
ggplot(data = rain_pred,
       aes(x = e)) +
  geom_histogram(bins = 20) +
  xlab("Residuals") +
  labs(title = "Histogram of residuals") +
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
              color = "red", linetype = "dashed", size = 1) +
    labs(caption = "95% confidence and prediction intervals for fitted line in weather data")
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
  labs(tag = "C") +
  labs(title = "Normal QQ-plot of residuals") +
  theme(text = element_text(size = 18))

# Histogram of the residuals:
ggplot(data = rain_pred,
       aes(x = e)) +
  geom_histogram(bins = 20) +
  xlab("Residuals") +
  labs(title = "Histogram of residuals") +
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
# beta_estimates <- model.mult_sum$coefficients

a <- exp(beta_estimates[1]) # a = 1.041
b <- exp(beta_estimates[2]) # b = 0.943

confint(a*b^5)

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
  labs(title = "Temperature vs rain") +
  theme(text = element_text(size = 18))

# temp vs pressure
ggplot(data = weather, 
       aes(x = temp, y = pressure)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0) +
  ylim(980,1040) +
  xlab("Temperature") +
  ylab("Pressure") +
  labs(title = "Temperature vs pressure") +
  theme(text = element_text(size = 18))

# pressure vs rain
ggplot(data = weather, 
       aes(x = pressure, y = log(rain))) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0) +
  xlab("Pressure") +
  ylab("Rain") +
  labs(title = "Pressure vs rain") +
  theme(text = element_text(size = 18))


#### 2 c) ####

# Fitting a linear regression model
(model.mult <- lm(log(rain) ~ temp + pressure, data = weather)) 
model.mult_sum <- summary(model.mult)

# Betas (B0 =  , B1 = )
beta_estimates <- model.mult_sum$coefficients
B0 <- beta_estimates[1]
B1 <- beta_estimates[2]

# Estimation of standard deviation
se_B0 <- model.mult_sum$coefficients[1,2]
se_B1 <- model.mult_sum$coefficients[2,2]

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
  labs(tag = "B") +
  labs(title = "Residuals vs predicted values Y-hat") +
  theme(text = element_text(size = 18))

## Plot resid vs pressure
ggplot(data = weather, 
       aes(x = pressure, y = rain_pred$e)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0) +
  expand_limits(y = rain_elims) +
  xlab("Pressure") +
  ylab("Residual") +
  labs(tag = "C") +
  labs(title = "Pressure vs residual") +
  theme(text = element_text(size = 18))

## Temp vs resid
ggplot(data = weather, 
       aes(x = temperature, y = rain_pred$e)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0) +
  expand_limits(y = rain_elims) +
  xlab("Temperature") +
  ylab("Residual") +
  labs(tag = "D") +
  labs(title = "Temperature vs residual") +
  theme(text = element_text(size = 18))


# Make a normal QQ-plot of residuals.
ggplot(data = rain_pred, 
       aes(sample = e)) +
  geom_qq(size = 3) +
  geom_qq_line() +
  labs(tag = "E") +
  labs(title = "Normal QQ-plot of residuals") +
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

#### 2 f) Change the b-variable to different beta and see how line 425 changes output. ####
a3^20
# 0.31 => decrease by 31%

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
  labs(tag = "B") +
  labs(title = "Residuals vs predicted values Y-hat") +
  theme(text = element_text(size = 18))

# Make a normal QQ-plot of residuals.
ggplot(data = rain_pred, 
       aes(sample = e)) +
  geom_qq(size = 3) +
  geom_qq_line() +
  labs(tag = "C") +
  labs(title = "Normal QQ-plot of residuals") +
  theme(text = element_text(size = 18))

# Histogram of the residuals:
ggplot(data = rain_pred,
       aes(x = e)) +
  geom_histogram(bins = 20) +
  xlab("Residuals") +
  labs(title = "Histogram of residuals") +
  theme(text = element_text(size = 18))


#### 2.4 Temperature, pressure and location #### 

####  2 m) + n) #### 
rm(list=ls())

load( file = "/home/neko/RWTH/Master/Erasmus/Vorlesungen/LinLog/R/weather.rda")
#load("~/Desktop/LinLog/Project_1/Data/weather.rda")

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
ggplot(pred_multiloc, aes(x = conf.fit, y = e, color = location)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  expand_limits(y = elim) +
  facet_wrap(~ location)

ggplot(pred_multiloc, aes(x =  temp * pressure, y = e, color = location)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  expand_limits(y = elim) +
  facet_wrap(~ location)

ggplot(pred_multiloc, aes(sample = e)) +
  geom_qq() + geom_qq_line() +
  expand_limits(y = elim)


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
  labs(tag = "B") +
  labs(title = "Residuals vs predicted values Y-hat") +
  theme(text = element_text(size = 18))

# Make a normal QQ-plot of residuals.
ggplot(data = pred_multiloc, 
       aes(sample = e)) +
  geom_qq(size = 3) +
  geom_qq_line() +
  labs(tag = "C") +
  labs(title = "Normal QQ-plot of residuals") +
  theme(text = element_text(size = 18))

# Histogram of the residuals:
ggplot(data = pred_multiloc,
       aes(x = e)) +
  geom_histogram(bins = 20) +
  xlab("Residuals") +
  labs(title = "Histogram of residuals") +
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

ggplot(w.diagnostics, aes(x = temp, y = v)) +
  geom_jitter(width = 1) +
  expand_limits(y = 0) +
  geom_hline(yintercept = 1/n) +
  geom_hline(yintercept = 2*pplus1.2n/n, 
             color = "red", linetype = "dashed", size = 1) +
  facet_wrap(~ location)
  
ggplot(w.diagnostics, aes(x = pressure, y = v)) +
  geom_jitter(width = 1) +
  expand_limits(y = 0) +
  geom_hline(yintercept = 1/n) +
  geom_hline(yintercept = 2*pplus1.2n/n, 
             color = "red", linetype = "dashed", size = 1) +
  facet_wrap(~ location)
# Uppsala has the most observations. For small data sets, individual points have a higher influence, so Lund und Abisko have higher leverages. 

#### 3 b) ####
I_high <- which(w.diagnostics$v > 0.026)
