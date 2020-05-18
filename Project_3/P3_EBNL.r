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

mod_1 <- glm.nb(new_cases ~ new_cases + day_nbr, offset(log(population)), data = fhm.data)
