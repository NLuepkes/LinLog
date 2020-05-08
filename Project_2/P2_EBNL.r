# Project 2

load( file = "/home/neko/RWTH/Master/Erasmus/Vorlesungen/LinLog/R/weather.rda")
#load("~/Desktop/LinLog/Project_1/Data/weather.rda")

weather$lowrain <- as.numeric(weather$rain < 25)

#### 1a) ####
mean(weather$lowrain) # 0.2868928
ones <- which(weather$lowrain == 1)

odds <- length(ones)/length(weather$lowrain) # 0.2868928
# 
logodds <- log(odds)

#### 1b) ####
nullmod <- glm(lowrain ~ 1, family = "binomial", data = weather)
sum_null <- summary(nullmod)
beta_est <- nullmod[["coefficients"]] # -0.9105233
confint(nullmod)
#     2.5 %    97.5 % 
# -1.042988 -0.780509
# Transform to odds to get corresponding 95 % for odds.
exp(beta_est)/(1+exp(beta_est)) #  0.2868928
stde <- sum_null$coefficients[1,2]

# ....
c(beta_est - 1.96*stde, beta_est + 1.96*stde) # (-1.0417150, -0.7793317)
# this is almost identical, so the estimation is really point on

#### Part 2 ####
#### 2a) ####
library(ggplot2)

(
  plot_2a <- ggplot(data = weather, 
                      aes(x = temp, y = lowrain)) + 
    geom_point(size = 1) +
    geom_smooth(method = loess) +
    xlab("Temperature") +
    ylab("Low rain")+
    theme(text = element_text(size = 16))
)
# it makes sense to have temp as a covariate because rain and temp are connected
# probability of having low/much rain differs when the temp differs (linearly)

#### 2b) ####
model_2b <- glm(lowrain ~ temp, family = "binomial", data=weather)
sum_2b <- summary(model_2b)
stde_1 <- sum_2b$coefficients[1,2]
stde_2 <- sum_2b$coefficients[2,2]
beta_est_2b <- model_2b[["coefficients"]]
beta_1 <- beta_est_2b[2]
confint(model_2b)

# Odds (exp(beta0)) and OR, odds ratio, exp(beta1)
exp(model_2b$coefficients)
(ci.or <- exp(ci.beta))


#                   2.5 %      97.5 %
# (Intercept) -0.73897322 -0.43949978
# temp        -0.09218264 -0.05599002
# Temperature is significant
exp(beta_est_2b)/(1+exp(beta_est_2b)) # (Intercept) 0.3569551 ; temp 0.4815429
exp(beta_1) # 0.9288 => odds decrease by 7% if temp increases by 1°C
exp(beta_1)^(-1) # 1.076658 => odds increase by 7% if temp decreases by 1°C

#### 2c) ####
x <- c(-10,-9,9,10)
w.x0 = data.frame(temp = x)
pred_2c <- cbind(w.x0, xb = predict(model_2b, w.x0, se.fit = TRUE))
# 1  -10   1.1618504
# 2   -9   1.0791266
# 3    9   0.2855435
# 4   10   0.2652129

# Look at pred_2c på gammalt hederligt sätt confint.

# C.I. for prediction for -10 C: -0.1293223  0.4293501
c(1.1618504 - 1.96*1.153174, 1.1618504 + 1.96*1.153174 ) 

# C.I. for prediction for -9 C: -0.1875203  0.3398244
c(1.079127 - 1.96*1.143995, 1.079127 + 1.96*1.143995 ) 

# C.I. for prediction for 9 C: -1.421375 -1.085347
c(0.2855435 - 1.96*1.089503, 0.2855435 + 1.96*1.089503 ) 

# C.I. for prediction for 10 C: 1.147929 1.506516
c(0.2652129 - 1.96*1.095791, 0.2652129 + 1.96*1.095791 ) 


#### 2 d) ####

# beta: log-odds(ratio) with c.i.:
model_2b$coefficients
(ci.beta <- confint(model_2b))

# Odds (exp(beta0)) and OR, odds ratio, exp(beta1)
exp(model_2b$coefficients)
(ci.or <- exp(ci.beta))

# predict for plotting #
# phat = estimated probabilities p
model_2b.pred <- cbind(
  weather,
  phat = predict(model_2b, type = "response"))

ggplot(model_2b.pred, aes(temp, lowrain)) +
  geom_point() +
  geom_smooth(se = FALSE, linetype = "dashed") +
  geom_line(aes(y = phat), color = "red", size = 1) +
  # geom_ribbon(aes(ymin = p.lwr, ymax = p.upr), alpha = 0.2) +
  xlab("Temperature") +
  ylab("Low rain") +
  #labs(title = "No rain (=1) or low rain (=0) vs temperature",
   #    caption = "red = fitted line, blue dashed = moving average") +
  theme(text = element_text(size = 14))

# logit = logodds with s.e. for constructing C.I.
model_2b.pred <- cbind(
  model_2b.pred,
  logit = predict(model_2b, se.fit = TRUE))
head(model_2b.pred)
# Remove unnecessary variable:
model_2b.pred$logit.residual.scale <- NULL

# Calculate confidence intervals for the log odds
# Standard normal quantile, i.e. 95 % confidence interval:
(lambda <- qnorm(1 - 0.05/2))
model_2b.pred$logit.lwr <- model_2b.pred$logit.fit - lambda*model_2b.pred$logit.se.fit
model_2b.pred$logit.upr <- model_2b.pred$logit.fit + lambda*model_2b.pred$logit.se.fit
head(model_2b.pred)

# Transform the log-odds intervals into C.I. for odds
model_2b.pred$odds.lwr <- exp(model_2b.pred$logit.lwr)
model_2b.pred$odds.upr <- exp(model_2b.pred$logit.upr)
head(model_2b.pred)

# Transform the odds intervals into C.I. for p
model_2b.pred$p.lwr <- model_2b.pred$odds.lwr/(1 + model_2b.pred$odds.lwr)
model_2b.pred$p.upr <- model_2b.pred$odds.upr/(1 + model_2b.pred$odds.upr)
head(model_2b.pred)

# Plotting the intervals:
ggplot(model_2b.pred, aes(temp, lowrain)) +
  geom_point() +
  geom_line(aes(y = phat), color = "red", size = 1) +
  geom_ribbon(aes(ymin = p.lwr, ymax = p.upr), alpha = 0.2) +
  xlab("Temperature") +
  ylab("Low rain") +
  #labs(title = "No rain (=1) or low rain (=0) vs temperature",
       # caption = "red = fitted line, with 95% confidence interval") +
  theme(text = element_text(size = 14))


#### 2 e) ####
# Leverage 
infl.weather <- influence(model_2b)
model_2b.pred <- cbind(weather,
                   xbeta = predict(model_2b),
                   v = infl.weather$hat)
head(model_2b.pred)

(plot.v <- ggplot(model_2b.pred, aes(temp, v)) + 
    geom_point() +
    geom_hline(yintercept = 1/1091, color = "blue", linetype = "dotted", size = 1) +
    geom_hline(yintercept = 6/1091, color = "blue", linetype = "dotted", size = 1) +
    facet_wrap(~ location) +
    labs(title = "Leverage vs linear predictor, by Y=0 or Y=1") +
    theme(text = element_text(size = 14)))

# Highlight unusually large ones (our choice):
I_highv <- which(model_2b.pred$v > 6/1091)
plot.v +
  geom_point(data = model_2b.pred[I_highv, ], size = 3, 
             color = "red", shape = 24) +
  labs(title = "Leverage vs linear predictor, by Y=0 or Y=1")

ggplot(model_2b.pred, aes(temp, v)) + 
  geom_point() +
  geom_point(data = model_2b.pred[I_highv, ], size = 3, 
             color = "red", shape = 24) +
  facet_wrap(~ location) +
  labs(title = "Leverage vs temp",
       caption = "blue = 2(p+1)/n and  1/n") +
  geom_hline(yintercept = 1/1091, color = "blue", linetype = "dotted", size = 1) +
  geom_hline(yintercept = 6/1091, color = "blue", linetype = "dotted", size = 1) +
  theme(text = element_text(size = 14))

# Use facet_grid to split rows and columns by different variables:
ggplot(model_2b.pred, aes(temp, v)) +
  geom_point() +
  geom_point(data = model_2b.pred[I_highv, ], color = "red",
             shape = 24, size = 3) +
  # facet_grid(rows = vars(lowrain), cols = vars(temp)) + How are we supposed to split here? facetwrap?
  # labs(title = "wind speed vs cars, by Y=0 or Y=1 and by temp diff",
  #     caption = "rows: Y = 0 or 1, columns: tempdiff") +
  theme(text = element_text(size = 14))


#### 2 f) ####

model_2b.pred$pearson <- infl.weather$pear.res
model_2b.pred$stdres <- model_2b.pred$pearson/sqrt(1 - model_2b.pred$v)
head(model_2b.pred)

ggplot(model_2b.pred, aes(sample = stdres)) +
  geom_qq() + geom_qq_line() +
  labs(title = "Q-Q-plot standardized residuals") +
  theme(text = element_text(size = 14))

# The as.factor(highpm10) prevents ggplot from using a 
# spectrum and instead use default color number 1 and 2.
ggplot(model_2b.pred, aes(xbeta, stdres, 
                      color = as.factor(lowrain))) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-2, 2), color = "red", linetype = "dashed",
             size = 1) +
  geom_hline(yintercept = c(-4, 4), color = "red", linetype = "dotted",
             size = 1) +
  labs(title = "Standardized residuals vs linear predictor",
       color = "Y") +
  theme(text = element_text(size = 14))

I_highstdres2 <- which(model_2b.pred$stdres^2 > 4)

ggplot(model_2b.pred, aes(xbeta, stdres^2, color = as.factor(lowrain))) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = 4, color = "red", linetype = "dashed",
             size = 1) +
  labs(title = "Squared standardized residuals vs linear predictor",
       color = "Y") +
  theme(text = element_text(size = 14))

# Deviance residuals, standardised
model_2b.pred$devres <- infl.weather$dev.res
model_2b.pred$devstd <- model_2b.pred$devres/sqrt(1 - model_2b.pred$v)
head(model_2b.pred)

ggplot(model_2b.pred, aes(xbeta, devstd, color = as.factor(lowrain))) +
  geom_point() +
  geom_point(data = model_2b.pred[I_highstdres2, ], size = 3, 
             color = "red", shape = 24) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-2, 2), color = "red", linetype = "dashed",
             size = 1) +
  geom_hline(yintercept = c(-4, 4), color = "red", linetype = "dotted",
             size = 1) +
  labs(title = "Standardized deviance residuals vs linear predictor",
       color = "Y") +
  theme(text = element_text(size = 14))

ggplot(model_2b.pred, aes(temp, devstd, color = as.factor(lowrain))) +
  geom_point() +
  geom_point(data = model_2b.pred[I_highstdres2, ], size = 3, 
             color = "red", shape = 24)+
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-2, 2), color = "red", linetype = "dashed",
             size = 1) +
  geom_hline(yintercept = c(-4, 4), color = "red", linetype = "dotted",
             size = 1) +
  labs(title = "Standardized deviance residuals vs temperature",
       color = "Y") +
  theme(text = element_text(size = 14)) +
  facet_wrap(~ location)

ggplot(model_2b.pred, aes(sample = devstd)) +
  geom_qq() + geom_qq_line()

#### 2 g) ####

model_2b.pred$Dcook <- cooks.distance(model_2b)
head(model_2b.pred)

ggplot(model_2b.pred, aes(xbeta, Dcook, color = as.factor(lowrain))) +
  geom_point() +
  geom_point(data = model_2b.pred[I_highv, ], color = "black",
             shape = 24, size = 3) +
  geom_hline(yintercept = 0) +
  #  geom_hline(yintercept = 1, color = "red", linetype = "dashed",
  #             size = 1) +
  geom_hline(yintercept = 4/1091, color = "red", linetype = "dotted",
             size = 1) +
  labs(title = "Cook's distance vs linear predictor",
       color = "Y") +
  theme(text = element_text(size = 14)) 
  #facet_grid(rows = vars(highpm10), cols = vars(tempdiff))

#### Part 3 ####

#### 3 a) ####

library(ggplot2)

(
  plot_3a <- ggplot(data = weather, 
                    aes(x = pressure, y = lowrain)) + 
    geom_point(size = 1) +
    geom_smooth(method = loess) +
    xlab("Pressure") +
    ylab("Low rain")+
    theme(text = element_text(size = 16))
)

model_3a <- glm(lowrain ~ I(pressure - 1012), family = "binomial", data=weather)
sum_3a <- summary(model_3a)

# beta: log-odds(ratio) with c.i.:
model_3a$coefficients
(ci.beta <- confint(model_3a))

# Odds (exp(beta0)) and OR, odds ratio, exp(beta1)
exp(model_3a$coefficients)
(ci.or <- exp(ci.beta))

# predict for plotting #
# phat = estimated probabilities p
model_3a.pred <- cbind(
  weather,
  phat = predict(model_3a, type = "response"))

ggplot(model_3a.pred, aes(I(pressure - 1012), lowrain)) +
  geom_point() +
  geom_smooth(se = FALSE, linetype = "dashed") +
  geom_line(aes(y = phat), color = "red", size = 1) +
  xlab("Pressure") +
  ylab("Low rain") +
  labs(title = "No rain (=1) or low rain (=0) vs Pressure",
       caption = "red = fitted line, blue dashed = moving average") +
  theme(text = element_text(size = 14))

# logit = logodds with s.e. for constructing C.I.
model_3a.pred <- cbind(
  model_3a.pred,
  logit = predict(model_3a, se.fit = TRUE))
head(model_3a.pred)
# Remove unnecessary variable:
model_3a.pred$logit.residual.scale <- NULL

# Calculate confidence intervals for the log odds
# Standard normal quantile, i.e. 95 % confidence interval:
(lambda <- qnorm(1 - 0.05/2))
model_3a.pred$logit.lwr <- model_3a.pred$logit.fit - lambda*model_3a.pred$logit.se.fit
model_3a.pred$logit.upr <- model_3a.pred$logit.fit + lambda*model_3a.pred$logit.se.fit
head(model_3a.pred)

# Transform the log-odds intervals into C.I. for odds
model_3a.pred$odds.lwr <- exp(model_3a.pred$logit.lwr)
model_3a.pred$odds.upr <- exp(model_3a.pred$logit.upr)
head(model_3a.pred)

# Transform the odds intervals into C.I. for p
model_3a.pred$p.lwr <- model_3a.pred$odds.lwr/(1 + model_3a.pred$odds.lwr)
model_3a.pred$p.upr <- model_3a.pred$odds.upr/(1 + model_3a.pred$odds.upr)
head(model_3a.pred)

# Plotting the intervals:
ggplot(model_3a.pred, aes(I(pressure - 1012), lowrain)) +
  geom_point() +
  geom_smooth(se = FALSE, linetype = "dashed") +
  geom_line(aes(y = phat), color = "red", size = 1) +
  geom_line(aes(y = phat), color = "red", size = 1) +
  geom_ribbon(aes(ymin = p.lwr, ymax = p.upr), alpha = 0.2) +
  xlab("Pressure") +
  ylab("Low rain") +
  labs(title = "No rain (=1) or low rain (=0) vs pressure",
       caption = "red = fitted line, with 95% confidence interval") +
  theme(text = element_text(size = 14))



#### 3 b) ####

# Leverage 
infl.weather <- influence(model_3a)
model_3a.pred <- cbind(weather,
                       xbeta = predict(model_3a),
                       v = infl.weather$hat)
head(model_3a.pred)

(plot.v <- ggplot(model_3a.pred, aes(I(pressure - 1012), v)) + 
    geom_point() +
    geom_hline(yintercept = 1/1091, color = "blue", linetype = "dotted", size = 1) +
    geom_hline(yintercept = 6/1091, color = "blue", linetype = "dotted", size = 1) +
    facet_wrap(~ location) +
    labs(title = "Leverage vs linear predictor, by Y=0 or Y=1") +
    theme(text = element_text(size = 14)))

# Highlight unusually large ones (our choice):
I_highv3a <- which(model_3a.pred$v > 6/1091)
plot.v +
  geom_point(data = model_3a.pred[I_highv3a, ], size = 3, 
             color = "red", shape = 24) +
  labs(title = "Leverage vs linear predictor, by Y=0 or Y=1")

ggplot(model_3a.pred, aes(I(pressure - 1012), v)) + 
  geom_point() +
  geom_point(data = model_3a.pred[I_highv3a, ], size = 3, 
             color = "red", shape = 24) +
  facet_wrap(~ location) +
  labs(title = "Leverage vs pressure",
       caption = "blue = 2(p+1)/n and  1/n") +
  geom_hline(yintercept = 1/1091, color = "blue", linetype = "dotted", size = 1) +
  geom_hline(yintercept = 6/1091, color = "blue", linetype = "dotted", size = 1) +
  theme(text = element_text(size = 14))

# Use facet_grid to split rows and columns by different variables:
ggplot(model_3a.pred, aes(I(pressure - 1012), v)) +
  geom_point() +
  geom_point(data = model_3a.pred[I_highv3a, ], color = "red",
             shape = 24, size = 3) +
  # facet_grid(rows = vars(lowrain), cols = vars(temp)) + How are we supposed to split here? facetwrap?
  # labs(title = "wind speed vs cars, by Y=0 or Y=1 and by temp diff",
       #caption = "rows: Y = 0 or 1, columns: tempdiff") +
  theme(text = element_text(size = 14))



#### 3 c) ####

model_3a.pred$pearson <- infl.weather$pear.res
model_3a.pred$stdres <- model_3a.pred$pearson/sqrt(1 - model_3a.pred$v)
head(model_3a.pred)

# Deviance residuals, standardised
model_3a.pred$devres <- infl.weather$dev.res
model_3a.pred$devstd <- model_3a.pred$devres/sqrt(1 - model_3a.pred$v)
head(model_3a.pred)

ggplot(model_3a.pred, aes(sample = stdres)) +
  geom_qq() + geom_qq_line() +
  labs(title = "Q-Q-plot standardized residuals") +
  theme(text = element_text(size = 14))

# The as.factor(highpm10) prevents ggplot from using a 
# spectrum and instead use default color number 1 and 2.
ggplot(model_3a.pred, aes(I(pressure - 1012), devres, 
                          color = as.factor(lowrain))) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-2, 2), color = "red", linetype = "dashed",
             size = 1) +
  geom_hline(yintercept = c(-4, 4), color = "red", linetype = "dotted",
             size = 1) +
  labs(title = "Standardized residuals vs linear predictor",
       color = "Y") +
  theme(text = element_text(size = 14))

I_highstdres2 <- which(model_3a.pred$stdres^2 > 20)

ggplot(model_3a.pred, aes(xbeta, stdres^2, color = as.factor(lowrain))) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = 4, color = "red", linetype = "dashed",
             size = 1) +
  labs(title = "Squared standardized residuals vs linear predictor",
       color = "Y") +
  theme(text = element_text(size = 14))

# Deviance residuals, standardised
model_3a.pred$devres <- infl.weather$dev.res
model_3a.pred$devstd <- model_3a.pred$devres/sqrt(1 - model_3a.pred$v)
head(model_3a.pred)

ggplot(model_3a.pred, aes(xbeta, devstd, color = as.factor(lowrain))) +
  geom_point() +
  geom_point(data = model_3a.pred[I_highstdres2, ], size = 3, 
             color = "red", shape = 24) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-2, 2), color = "red", linetype = "dashed",
             size = 1) +
  geom_hline(yintercept = c(-4, 4), color = "red", linetype = "dotted",
             size = 1) +
  labs(title = "Standardized deviance residuals vs linear predictor",
       color = "Y") +
  theme(text = element_text(size = 14))

ggplot(model_3a.pred, aes( I(pressure - 1012), devstd, color = as.factor(lowrain))) +
  geom_point() +
  geom_point(data = model_3a.pred[I_highstdres2, ], size = 3, 
             color = "red", shape = 24)+
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-2, 2), color = "red", linetype = "dashed",
             size = 1) +
  geom_hline(yintercept = c(-4, 4), color = "red", linetype = "dotted",
             size = 1) +
  labs(title = "Standardized deviance residuals vs temp",
       color = "Y") +
  theme(text = element_text(size = 14)) +
  facet_wrap(~ location)

ggplot(model_3a.pred, aes(sample = devstd)) +
  geom_qq() + geom_qq_line()


#### 3 d) ####


model_3a.pred$Dcook <- cooks.distance(model_3a)
head(model_3a.pred)

ggplot(model_3a.pred, aes(I(pressure - 1012), Dcook, color = as.factor(lowrain))) +
  geom_point() +
  geom_point(data = model_3a.pred[I_highv3a, ], color = "black",
             shape = 24, size = 3) +
  geom_hline(yintercept = 0) +
  #  geom_hline(yintercept = 1, color = "red", linetype = "dashed",
  #             size = 1) +
  geom_hline(yintercept = 4/1091, color = "red", linetype = "dotted",
             size = 1) +
  labs(title = "Cook's distance vs linear predictor",
       color = "Y") +
  theme(text = element_text(size = 14)) 
#facet_grid(rows = vars(highpm10), cols = vars(tempdiff))
#### 3 f + g ####

bic <- BIC(nullmod,model_2b, model_3a)
aic <- AIC(nullmod,model_2b, model_3a)
(collect_AIC <- data.frame(aic, bic))

# model 3: with cars and zerodiff is the best (BIC)

# pseudo R2####+
logLik(nullmod)
(lnL0 <- logLik(nullmod)[1])
(R2CS_max <- 1 - (exp(lnL0))^(2/1091))
# Collect the log likelihoods L(betahat)
collect_AIC$loglik <- 
  c(logLik(nullmod)[1],
    logLik(model_2b)[1],
    logLik(model_3a)[1])
# calculate R2_CS:
collect_AIC$R2CS <- 1 - (exp(lnL0 - collect_AIC$loglik))^(2/1091)
# Canculate R2_N:
collect_AIC$R2N <- collect_AIC$R2CS/R2CS_max

# Show them as % with one decimal value:
round(100*collect_AIC[, c("R2CS", "R2N")], digits = 1)



#### 4 a) ####

weather$location <- relevel(weather$location, "Uppsala")

model_4a <- glm(lowrain ~ I(pressure - 1012)*temp + location, family = "binomial", data = weather)
sum_4a <- summary(model_4a)

anova(model_2b,model_4a)
anova(model_3a,model_4a)

# beta: log-odds(ratio) with c.i.:
model_4a$coefficients
(ci.beta <- confint(model_4a))

# Odds (exp(beta0)) and OR, odds ratio, exp(beta1)
exp(model_4a$coefficients)
(ci.or <- exp(ci.beta))

# predict for plotting #
# phat = estimated probabilities p
model_4a.pred <- cbind(
  weather,
  phat = predict(model_4a, type = "response"))


#### 4 b) ####

library(ggplot2)

ggplot(model_4a.pred, aes(I(pressure - 1012), lowrain)) +
  geom_point() +
  geom_smooth(se = FALSE, linetype = "dashed") +
  geom_point(aes(y = phat, color = temp)) + scale_color_viridis_c() +
  facet_wrap(~ location) +
  xlab("Pressure") +
  ylab("Low rain") +
  labs(title = "No rain (=1) or low rain (=0) vs Pressure")
  theme(text = element_text(size = 14))

ggplot(model_4a.pred, aes(temp, lowrain)) +
  geom_point() +
  geom_smooth(se = FALSE, linetype = "dashed") +
  geom_point(aes(y = phat, color = pressure)) + scale_color_viridis_c() +
  facet_wrap(~ location) +
  xlab("Temperature") +
  ylab("Low rain") +
  labs(title = "No rain (=1) or low rain (=0) vs temperature")
  theme(text = element_text(size = 14))
# Se hur trycket vs temp följer s-kurvan

#### 4 c) ???? ####
  # Air pressure, because it looks more regular than for temperature.
  
  I_lund <- which(weather$location == "Lund")
  
  model_4c <- glm(lowrain ~ I(pressure - 1012)*temp + location, family = "binomial", data = weather[I_lund,])
  sum_4c <- summary(model_4c)
  
  # beta: log-odds(ratio) with c.i.:
  model_4c$coefficients
  (ci.beta <- confint(model_4c))
  
  # Odds (exp(beta0)) and OR, odds ratio, exp(beta1)
  exp(model_4c$coefficients)
  (ci.or <- exp(ci.beta))
  
  step(model_4c, k = log(nrow(weather)))
  # Is this correctly done? How do I interpret the output in the console?
  
  
#### 4 d) ####
  
  # predict for plotting #
  # phat = estimated probabilities p
  model_4a.pred <- cbind(
    weather,
    phat = predict(model_4a, type = "response"))
  
  infl.weather <- influence(model_4c)
  model_4a.pred <- cbind(weather,
                         xbeta = predict(model_4a),
                         v = infl.weather$hat)
  head(model_4a.pred)
  
  
  # Deviance residuals, standardised
  model_4a.pred$devres <- infl.weather$dev.res
  model_4a.pred$devstd <- model_4a.pred$devres/sqrt(1 - model_4a.pred$v)
  head(model_4a.pred)
  
  model_4a.pred$pearson <- infl.weather$pear.res
  model_4a.pred$stdres <- model_4a.pred$pearson/sqrt(1 - model_4a.pred$v)
  head(model_4a.pred)
  
  ggplot(model_4a.pred, aes(sample = devres)) +
    geom_qq() + geom_qq_line() +
    labs(title = "Q-Q-plot standardized deviance residuals") +
    theme(text = element_text(size = 14))
  
  ggplot(model_4a.pred, aes(xbeta, devres, 
                            color = as.factor(lowrain))) +
    geom_point() +
    facet_wrap(~ location) +
    geom_hline(yintercept = 0) +
    geom_hline(yintercept = c(-2, 2), color = "red", linetype = "dashed",
               size = 1) +
    geom_hline(yintercept = c(-4, 4), color = "red", linetype = "dotted",
               size = 1) +
    labs(title = "Standardized deviance residuals vs linear predictor",
         color = "Y") +
    theme(text = element_text(size = 14))
  
  
  ggplot(model_4a.pred, aes(temp, devres, 
                            color = as.factor(lowrain))) +
    geom_point() +
    facet_wrap(~ location) +
    # geom_point(aes(y = ????, color = pressure)) + scale_color_viridis_c()+
    geom_hline(yintercept = 0) +
    geom_hline(yintercept = c(-2, 2), color = "red", linetype = "dashed",
               size = 1) +
    geom_hline(yintercept = c(-4, 4), color = "red", linetype = "dotted",
               size = 1) +
    labs(title = "Standardized deviance residuals vs linear predictor",
         color = "Y") +
    theme(text = element_text(size = 14))
  
  
  ggplot(model_4a.pred, aes(pressure, stdres, 
                            color = as.factor(lowrain))) +
    geom_point() +
    facet_wrap(~ location) +
    # geom_point(aes(y = ????, color = temp)) + scale_color_viridis_c()+
    geom_hline(yintercept = 0) +
    geom_hline(yintercept = c(-2, 2), color = "red", linetype = "dashed",
               size = 1) +
    geom_hline(yintercept = c(-4, 4), color = "red", linetype = "dotted",
               size = 1) +
    labs(title = "Standardized residuals vs linear predictor",
         color = "Y") +
    theme(text = element_text(size = 14))
  
# ----------------------------------------   Above is the problematic part
  
  ggplot(model_4a.pred, aes(xbeta, stdres^2, color = as.factor(lowrain))) +
    geom_point() +
    geom_hline(yintercept = 0) +
    geom_hline(yintercept = 4, color = "red", linetype = "dashed",
               size = 1) +
    labs(title = "Squared standardized residuals vs linear predictor",
         color = "Y") +
    theme(text = element_text(size = 14))
  
  # Deviance residuals, standardised
  model_4a.pred$devres <- infl.weather$dev.res
  model_4a.pred$devstd <- model_4a.pred$devres/sqrt(1 - model_4a.pred$v)
  head(model_4a.pred)
  
  I_highstdres4 <- which(model_4a.pred$stdres^2 > 20)
  
  ggplot(model_4a.pred, aes(xbeta, devstd, color = as.factor(lowrain))) +
    geom_point() +
    geom_point(data = model_4a.pred[I_highstdres4, ], size = 3, 
               color = "red", shape = 24) +
    geom_hline(yintercept = 0) +
    geom_hline(yintercept = c(-2, 2), color = "red", linetype = "dashed",
               size = 1) +
    geom_hline(yintercept = c(-4, 4), color = "red", linetype = "dotted",
               size = 1) +
    labs(title = "Standardized deviance residuals vs linear predictor",
         color = "Y") +
    theme(text = element_text(size = 14))
  
  ggplot(model_4a.pred, aes( I(pressure - 1012), devstd, color = as.factor(lowrain))) +
    geom_point() +
    geom_point(data = model_4a.pred[I_highstdres4, ], size = 3, 
               color = "red", shape = 24)+
    geom_hline(yintercept = 0) +
    geom_hline(yintercept = c(-2, 2), color = "red", linetype = "dashed",
               size = 1) +
    geom_hline(yintercept = c(-4, 4), color = "red", linetype = "dotted",
               size = 1) +
    labs(title = "Standardized deviance residuals vs temp",
         color = "Y") +
    theme(text = element_text(size = 14)) +
    facet_wrap(~ location)
  
  ggplot(model_4a.pred, aes(sample = devstd)) +
    geom_qq() + geom_qq_line()
  
#### 4 e) ####

  model_4a.pred$Dcook <- cooks.distance(model_4a)
  head(model_4a.pred)
  
  ggplot(model_4a.pred, aes(xbeta, Dcook, color = as.factor(lowrain))) +
    geom_point() +
    geom_point(data = model_4a.pred[I_highstdres4, ], color = "black",
               shape = 24, size = 3) +
    geom_hline(yintercept = 0) +
    #  geom_hline(yintercept = 1, color = "red", linetype = "dashed",
    #             size = 1) +
    geom_hline(yintercept = 14/1091, color = "red", linetype = "dotted",
               size = 1) +
    labs(title = "Cook's distance vs linear predictor",
         color = "Y") +
    theme(text = element_text(size = 14)) 
  #facet_grid(rows = vars(highpm10), cols = vars(tempdiff))
#### 4 f + g ####
  
  
  
  bic <- BIC(nullmod,model_2b, model_3a, model_4a)
  aic <- AIC(nullmod,model_2b, model_3a, model_4a)
  (collect_AIC <- data.frame(aic, bic))
  
  # model ???: ???? is the best (BIC)
  
  # pseudo R2####+
  logLik(nullmod)
  (lnL0 <- logLik(nullmod)[1])
  (R2CS_max <- 1 - (exp(lnL0))^(2/1091))
  # Collect the log likelihoods L(betahat)
  collect_AIC$loglik <- 
    c(logLik(nullmod)[1],
      logLik(model_2b)[1],
      logLik(model_3a)[1],
      logLik(model_4a)[1])
  # calculate R2_CS:
  collect_AIC$R2CS <- 1 - (exp(lnL0 - collect_AIC$loglik))^(2/1091)
  # Canculate R2_N:
  collect_AIC$R2N <- collect_AIC$R2CS/R2CS_max
  
  # Show them as % with one decimal value:
  round(100*collect_AIC[, c("R2CS", "R2N")], digits = 1)
  
 #### Section 5 ####
 #### 5a) ####
 model_2b
 model_3a
 model_4a
 
 pred.phat <- cbind(
  weather,
  p.2 = predict(model_2b, type = "response"),
  p.3 = predict(model_3a, type = "response"),
  p.4 = predict(model_4a, type = "response"))
 head(pred.phat)
 
# Confusion matrix for all models
# Calculate Y-hat using all models

pred.phat$yhat.2 <- as.numeric(pred.phat$p.2 > 0.5)
pred.phat$yhat.3 <- as.numeric(pred.phat$p.3 > 0.5)
pred.phat$yhat.4 <- as.numeric(pred.phat$p.4 > 0.5)

(row.01 <- table(weather$lowrain))

(col.01.2 <- table(pred.phat$yhat.2))
(confusion.2 <- table(pred.phat$lowrain, pred.phat$yhat.2))
(spec.2 <- confusion.2[1, 1] / row.01[1])
(sens.2 <- confusion.2[2, 2] / row.01[2])
(accu.2 <- sum(diag(confusion.2)) / sum(confusion.2))
(prec.2 <- confusion.2[2, 2] / col.01.2[2])

(col.01.3 <- table(pred.phat$yhat.3))
(confusion.3 <- table(pred.phat$lowrain, pred.phat$yhat.3))
(spec.3 <- confusion.3[1, 1] / row.01[1])
(sens.3 <- confusion.3[2, 2] / row.01[2])
(accu.3 <- sum(diag(confusion.3)) / sum(confusion.3))
(prec.3 <- confusion.3[2, 2] / col.01.3[2])

(col.01.4 <- table(pred.phat$yhat.4))
(confusion.4 <- table(pred.phat$lowrain, pred.phat$yhat.4))
(spec.4 <- confusion.4[1, 1] / row.01[1])
(sens.4 <- confusion.4[2, 2] / row.01[2])
(accu.4 <- sum(diag(confusion.4)) / sum(confusion.4))
(prec.4 <- confusion.4[2, 2] / col.01.3[2])


#### 5b) ####
