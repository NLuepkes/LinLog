# Project 3A
rm(list=ls())

load( file = "/home/neko/RWTH/Master/Erasmus/Vorlesungen/LinLog/Project/LinLog/Project_3/Data/fhm_data.RData")

library(ggplot2)
# Necessary for reading data in Stata format, .dta:
library(foreign)
# Necessary for multinomial regression:
library(nnet)
# Necessary for ordinal regression:
library(MASS)
# Necessary for quantile regresssion:
library(quantreg)

summary(fhm.data)

(
  plot_data <- ggplot(data = fhm.data, 
                      aes(x = obs_date, y = new_cases)) + 
    geom_point(size = 1) +
    xlab("observation data") +
    ylab("new cases")+
    theme(text = element_text(size = 16))
)
(
  plot_data + 
    geom_smooth(method = lm, se = FALSE) +
    labs(caption = "fhm data and fitted line")
)

fhm.data$region <- factor(fhm.data$region) 
fhm.data$region <- relevel(fhm.data$region, "Stockholm")

mod_1 <- glm.nb(new_cases ~ day_nbr_region + day_nbr, offset(log(population)), data = fhm.data)
summary(mod_1)
step(mod_1, k=log(nrow(fhm.data)))

mod_1.coeff <- mod_1$coefficients
confint(mod_1)

# basic residual analysis
fhm_pred <- 
  cbind(fhm.data, 
        pred = predict(mod_1, interval = "prediction"))

fhm_pred$e <- mod_1$residuals
# Save the max-value in order to make the y-axis symmetrical in the plots.
(max.e <- max(abs(fhm_pred$e)))
(fhm_elims <- c(-max.e, max.e))

# plot confidence intervall and prediction intervall

mod_2 <- glm.nb(new_cases ~ (day_nbr_region)*(day_nbr)*obs_date + region, offset(log(population)), data = fhm.data)
summary(mod_2)
step(mod_2, k=log(nrow(fhm.data)))
