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

fhm.data$region <- factor(fhm.data$region) 
fhm.data$region <- relevel(fhm.data$region, "Stockholm")

mod_1 <- glm.nb(new_cases ~ day_nbr_region + day_nbr, offset(log(population)), data = fhm.data)
summary(mod_1)
step(mod_1, k=log(nrow(fhm.data)))

 mod_1.coeff <- mod_1$coefficients

# plot confidence intervall and prediction intervall

mod_2 <- glm.nb(new_cases ~ (day_nbr_region)*(day_nbr)*obs_date + region, offset(log(population)), data = fhm.data)
summary(mod_2)
step(mod_2, k=log(nrow(fhm.data)))
