# Project 1

########### 1. a) ########### 


#load( file = "RWTH/Master/Erasmus/Vorlesungen/LinLog/R/weather.rda")
load("~/Desktop/LinLog/Project_1/Data/weather.rda")

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

average_start <- B0 + B1*0 # Average at the beginning = 37.500 + 1.3 (mm/C)

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

#### Basic residual analysis ####

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


########### 1. b) ########### 


###############################################################################

rm(list=ls())
#load( file = "RWTH/Master/Erasmus/Vorlesungen/LinLog/R/weather.rda")
load("~/Desktop/LinLog/Project_1/Data/weather.rda")
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

#### Basic residual analysis ####

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


########### 1. c) ########### 

# Discussion part, use histograms, QQ-plot to argue why it was good/bad!
# Good link for transformations: https://rcompanion.org/handbook/I_12.html
# Tried sqrt/log/cubic-transformations. The best one is when the residual looks normal distributed.

########### 1. d) ########### 

########### 1. e) ########### 
# See above plots.

########### 1. f) ########### 

# Get confidence and prediction interval for x0 = 5 degrees celsius
mm_x0 <- data.frame(x = c(5))

(rain_pred_y0 <- cbind(mm_x0,
                     pred = predict(model, mm_x0,
                                    interval = "prediction")))

###############################################################################

rm(list=ls())

#load( file = "RWTH/Master/Erasmus/Vorlesungen/LinLog/R/weather.rda")
load("~/Desktop/LinLog/Project_1/Data/weather.rda")


#install.packages("plotly")

# 2 b)

library(plotly)

fig <- plot_ly(weather, x = ~temp, y = ~rain, z = ~pressure,
               marker = list(color = ~mpg, colorscale = c('#FFE1A1', '#683531','#683531'), showscale = FALSE))
fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'Temperature'),
                                   yaxis = list(title = 'Precipitation'),
                                   zaxis = list(title = 'Pressure'))
                     )
fig


# 2 c)

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


# 2 d) 

rain_pred <- 
  cbind(weather, 
        pred = predict(model.mult, interval = "prediction"))

########## Basic residual analysis ##########

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

## 2 e)


# Level of rain over temp + pressure
# y = B0 + B1 * x + epsilon <-> log(Y) = B0 + B1 * x + epsilon
# Transformation -> Y = exp(B0) * exp(B1)^x * exp(epsilon) = a * b^x * exp(epsilon),
# where a = exp(B0) and b = exp(B1)

# Estimates of a and b
beta_estimates <- model.mult_sum$coefficients
a <- exp(beta_estimates[2]) # a = 1.041
b <- exp(beta_estimates[3]) # b = 0.943

# 95% confidence intervals
log_confInt <- exp(confInt)

# Average precipitation change by increasing with 1 degree.
a * b^1

## 2 f) Change the b-variable to different beta and see how line 425 changes output.
