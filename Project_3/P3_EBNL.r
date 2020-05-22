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

# look at raw data
ggplot(data = fhm.data, 
                      aes(x = obs_date, y = new_cases)) + 
    geom_point(size = 1) +
    xlab("observation date") +
    ylab("new cases")+
    theme(text = element_text(size = 16))+
    facet_wrap(~ region)


# refactor
fhm.data$region <- factor(fhm.data$region) 
fhm.data$region <- relevel(fhm.data$region, "Västra Götaland") #see summary

 ggplot(data = fhm.data, 
                    aes(x = day_nbr, y = new_cases)) + 
    geom_point(size = 1) +
    geom_vline(xintercept=10, col="blue") +
    xlab("day number") +
    ylab("new cases")+
    theme(text = element_text(size = 16))+
    facet_wrap(~ region)   
    
#### 1a) ####
# TODO: assign the same label through out the same region
fhm.data$late <- as.numeric(fhm.data$day_nbr > 10) 

summary(fhm.data)

# in the following, we try two different models
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

ggplot(data = fhm_pred, 
       aes(x = pred, y = e)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0) +
  expand_limits(y = fhm_elims) +
  xlab("Predicted day numbers") +
  ylab("Residual") +
  labs(title = "Residuals vs predicted values Y-hat") +
  theme(text = element_text(size = 18)) +
  facet_wrap(~ region)

# TODO plot confidence intervall and prediction intervall

mod_2 <- glm.nb(new_cases ~ (day_nbr_region)*(day_nbr)*obs_date + region, offset(log(population)), data = fhm.data)
summary(mod_2)
step(mod_2, k=log(nrow(fhm.data)))

# basic residual analysis
fhm_pred <- 
  cbind(fhm.data, 
        pred = predict(mod_2, interval = "prediction"))

fhm_pred$e <- mod_1$residuals
# Save the max-value in order to make the y-axis symmetrical in the plots.
(max.e <- max(abs(fhm_pred$e)))
(fhm_elims <- c(-max.e, max.e))

ggplot(data = fhm_pred, 
       aes(x = pred, y = e)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0) +
  expand_limits(y = fhm_elims) +
  xlab("Predicted day numbers") +
  ylab("Residual") +
  labs(title = "Residuals vs predicted values Y-hat") +
  theme(text = element_text(size = 18)) +
  facet_wrap(~ region)
# stockholm, östergötland, sörmland, and västmanland look special.
# let's look at outliers and stuff

# Leverage 
infl.fhm <- influence(mod_2)
mod_2.pred <- cbind(fhm.data,
                   xbeta = predict(mod_2),
                   v = infl.fhm$hat)
head(mod_2.pred)

(plot.v <- ggplot(mod_2.pred, aes(new_cases, v)) + 
    geom_point() +
    geom_hline(yintercept = 1/1367, color = "blue", linetype = "dotted", size = 1) +
    geom_hline(yintercept = (4+1)*2/1367, color = "blue", linetype = "dotted", size = 1) +
    facet_wrap(~ region) +
    labs(title = "Leverage vs linear predictor") +
    theme(text = element_text(size = 14)))
# they look weird. But one can see that they are higher for Stockholm and VästraGötaland    
    
#### 1b) ####
mod_nb <- glm.nb(new_cases ~ (day_nbr_region)*(day_nbr) + region, offset(log(population)), data = fhm.data)
mod_po <- glm(new_cases ~ (day_nbr_region)*(day_nbr) + region, offset(log(population)), data = fhm.data, family=poisson)
step(mod_po, k=log(nrow(fhm.data)))

# leverage
infl.fhm <- influence(mod_nb)
mod_nb.pred <- cbind(fhm.data,
                   xbeta = predict(mod_nb),
                   v = infl.fhm$hat,
                   phat = predict(mod_nb, type="response"))

# deviance residuals 
mod_nb.pred$devres <- infl.fhm$dev.res
mod_nb.pred$devstd <- mod_nb.pred$devres/sqrt(1 - mod_nb.pred$v)
head(mod_nb.pred)

I_late <- which(mod_nb$late == 1)

ggplot(mod_nb.pred, aes(xbeta, devstd, color = as.factor(late))) +
  geom_point() +
  #geom_point(data = mod_nb.pred[I_late, ], size = 3, 
  #           color = "red", shape = 24) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-2, 2), color = "red", linetype = "dashed",
             size = 1) +
  geom_hline(yintercept = c(-4, 4), color = "red", linetype = "dotted",
             size = 1) +
  labs(color = "Y") +
  theme(text = element_text(size = 14)) +
    facet_wrap(~ region)
  # we only have six regions with early onsets and their leverages are very high. 
  # This might be due to the fact that because the majority of the regions are late in onset, the model sees that more often.
  
  # one examplary plot for this beaviour:
  I_stk <- which(mod_nb.pred$region == "Stockholm")
  mod_nb.pred.stk <- mod_nb.pred[I_stk,]
  ggplot(mod_nb.pred.stk, aes(xbeta, devstd, color = as.factor(late))) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-2, 2), color = "red", linetype = "dashed",
             size = 1) +
  geom_hline(yintercept = c(-4, 4), color = "red", linetype = "dotted",
             size = 1) +
  labs(color = "Y") +
  theme(text = element_text(size = 14))
  
  # possible TODO 
  # cook's D
  # ROC
  # R^2
  # Is there a way to compare two different models that are completey different? Compare regular residuals? Max resisdulas?
  
  mod_po.pred <- cbind(fhm.data,
                   xbeta = predict(mod_po),
                   v = influence(mod_po)$hat,
                   phat = predict(mod_po, type="response"))
  
  # ROC
  library(pROC)
  mod_nb.roc <- roc(new_cases ~ phat, data = mod_nb.pred)
  mod_nb.roc.df <- coords(mod_nb.roc, transpose = FALSE)
  mod_nb.roc.df$model <- "nb"
  mod_po.roc <- roc(new_cases ~ phat, data = mod_po.pred)
  mod_po.roc.df <- coords(mod_po.roc, transpose = FALSE)
  mod_po.roc.df$model <- "po"
  
  
  ggroc(mod_nb.roc) +
  geom_abline(intercept = 1, slope = 1, linetype = "dashed") +
  coord_fixed() +
  labs(title = "ROC-curve for model nb")
  
  ggroc(mod_po.roc) +
  geom_abline(intercept = 1, slope = 1, linetype = "dashed") +
  coord_fixed() +
  labs(title = "ROC-curve for model po")
  
  roc.df.ideal <- data.frame(sensitivity = c(0, 1, 1),
                           specificity = c(1, 1, 0),
                           threshold = c(NA, NA, NA))
roc.df.ideal$model <- "ideal"

roc.df.line <- data.frame(sensitivity = c(0, 0.5, 1),
                           specificity = c(1, 0.5, 0),
                           threshold = c(NA, NA, NA))
roc.df.line$model <- "line"
  
  roc.df <- rbind(mod_po.roc.df, mod_nb.roc.df, roc.df.ideal, roc.df.line)
# Plot all the curves, in different colors:
ggplot(roc.df, aes(specificity, sensitivity,
                            color = model)) +
  geom_path(size = 1) +
  coord_fixed() +       # square plotting area
  scale_x_reverse() +   # Reverse scale on the x-axis!
  labs(title = "ROC-curves for all the models") +
  theme(text = element_text(size = 14))

  
  # AUC
  # Compare the AUC for the models:

roc.test(mod_po.roc, mod_nb.roc)
# AUC of roc1 AUC of roc2 
#  0.5932225   0.6014123

  #### see how well our models work
  mod_nb.pred <- cbind(
  fhm.data,
  mhat = predict(mod_nb, type = "response"))


  ggplot(mod_nb.pred, aes(day_nbr, new_cases, color = as.factor(late))) +
  geom_point() +
  #geom_point(data = mod_nb.pred[I_late, ], size = 3, 
  #           color = "red", shape = 24) +
  geom_line(aes(y = mhat)) +
  labs(title = "negative bin predictor",
       color = "Y") +
  theme(text = element_text(size = 14)) +
  facet_wrap(~ region)
  
  ggplot(mod_nb.pred, aes(day_nbr, (new_cases/population*100000), color = as.factor(late))) +
  geom_point() +
  #geom_point(data = mod_nb.pred[I_late, ], size = 3, 
  #           color = "red", shape = 24) +
  geom_line(aes(y = mhat/population*100000, color = 'green')) +
  labs(title = "negative bin predictor",
       color = "Y") +
  theme(text = element_text(size = 14)) +
  facet_wrap(~ region)
  
  # now look at the poission model  
  mod_po.pred <- cbind(
  fhm.data,
  mhat = predict(mod_po, type = "response"))

  ggplot(mod_po.pred, aes(day_nbr, (new_cases/population*100000), color = as.factor(late))) +
  geom_point() +
  #geom_point(data = mod_nb.pred[I_late, ], size = 3, 
  #           color = "red", shape = 24) +
  geom_line(aes(y = mhat/population*100000, color = 'green')) +
  labs(title = "poisson predictor",
       color = "") +
  theme(text = element_text(size = 14)) +
  facet_wrap(~ region)
  
  # residual analysis for both nb and po
  
  
  
  
#### now look at Östergötland ####
# different encodings for 'ö' make the == 'Östergötland' unusable, so we pick its name manually
fhm.my_og <- fhm.data[706,]
my_og <- fhm.my_og$region
I_og <- which(fhm.data$region == my_og)

fhm.data.og <- fhm.data[I_og,]

ggplot(data = fhm.data.og, 
                      aes(x = obs_date, y = new_cases)) + 
    geom_point(size = 1) +
    xlab("observation date") +
    ylab("new cases")+
    labs(title = "OG")+
    theme(text = element_text(size = 16))
 
mod_nb.pred.og <- mod_nb.pred[I_og,]
    
ggplot(mod_nb.pred.og, aes(day_nbr, new_cases, color = as.factor(late))) +
  geom_point() +
  #geom_point(data = mod_nb.pred[I_late, ], size = 3, 
  #           color = "red", shape = 24) +
  geom_line(aes(y = mhat)) +
  labs(color = "late") +
  theme(text = element_text(size = 14)) +
  facet_wrap(~ region)    

#fhm.data.og <- fhm.data.og[3,] #the first two data points are zero
  
mod_nb.og <- lm(new_cases ~ day_nbr + I(3*day_nbr^2) , data = fhm.data.og)
mod_po.og <- glm(new_cases ~ (day_nbr_region)*(day_nbr), data = fhm.data.og, family="poisson")

mod_nb.og.pred <- cbind(
  fhm.data.og,
  mhat = predict(mod_nb.og, type = "response"))
  
ggplot(mod_nb.og.pred, aes(day_nbr, new_cases)) +
  geom_point() +
  geom_line(aes(y = mhat)) +
  labs(title = "negative bin predictor",
       color = "Y") +
  theme(text = element_text(size = 14)) 
  
  
  
library(fitdistrplus)
FIT <- fitdist(fhm.data.og$new_cases, "norm")    ## note: it is "norm" not "normal"
class(FIT)
# [1] "fitdist"

plot(FIT)    ## use method `plot.fitdist`


# it's not done yet, but we leave it
