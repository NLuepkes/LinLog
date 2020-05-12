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

exp(confint(nullmod))/(1+exp(confint(nullmod)))

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
#   temp      xb.fit  xb.se.fit xb.residual.scale
# 1  -10  0.15001387 0.14251847                 1
# 2   -9  0.07615205 0.13452670                 1
# 3    9 -1.25336073 0.08572146                 1
# 4   10 -1.32722255 0.09147615                 1


# same transformation as in 1b)
# only on xb.fit
exp(pred_2c$xb.fit)/(1+exp(pred_2c$xb.fit))
# 0.5374333 0.5190288 0.2221189 0.2096192

# -10
c(0.5374333 - 1.96*0.14251847, 0.5374333 + 1.96*0.14251847)
# -9
c(0.5190288 - 1.96*0.13452670, 0.5190288 + 1.96*0.13452670)
# 9
c(0.2221189 - 1.96*0.08572146, 0.2221189 + 1.96*0.08572146)
# 10
c(0.2096192 - 1.96*0.09147615, 0.2096192 + 1.96*0.09147615)
 


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

#### 4 c) ####
  # Air pressure, because it looks more regular than for temperature.
  
  I_lund <- which(weather$location == "Lund")
  
  weather.Lund <- weather[I_lund,]
  
  model_4c <- glm(lowrain ~ I(pressure - 1012)*temp, family = "binomial", data = weather.Lund)
  sum_4c <- summary(model_4c)
  
  # beta: log-odds(ratio) with c.i.:
  model_4c$coefficients
  (ci.beta <- confint(model_4c))
  
  # Odds (exp(beta0)) and OR, odds ratio, exp(beta1)
  exp(model_4c$coefficients)
  (ci.or <- exp(ci.beta))
  
  step(model_4c, k = log(nrow(weather)))
  # Is this correctly done? How do I interpret the output in the console?
# Start:  AIC=169.06
# lowrain ~ I(pressure - 1012) * temp <- Model that is being tested

#                           Df Deviance    AIC
# - I(pressure - 1012):temp  1   143.74 164.72  <- the interaction term between pressure and temp is removed
# <none>                         141.08 169.06  <- nothing is done
# so now step() compares the two BIC values (AIC in output) and picks the model with the lower one and continues.
# Step:  AIC=164.72
# lowrain ~ I(pressure - 1012) + temp    <- current model

#                      Df Deviance    AIC
# - temp                1   147.63 161.62 <- remove temp
# <none>                    143.74 164.72 <- do nothing
# - I(pressure - 1012)  1   188.98 202.97  <- remove pressure
# removing temp lowered the BIC value (AIC in output), so step() continues with this
# Step:  AIC=161.62
# lowrain ~ I(pressure - 1012) <- current model
# 
#                      Df Deviance    AIC
# <none>                    147.63 161.62 <- do nothing
# - I(pressure - 1012)  1   194.37 201.36 <- remove pressure
# removing pressure doesn't give a better BIC value (AIC in output). Because it is the last term and there is nothing else 
# that could possibly be removed, step() stops here and presents the final model
  
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
    theme(text = element_text(size = 14))
  
  ggplot(model_4a.pred, aes(temp, devres)) +
    geom_point() +
    facet_wrap(~ location) +
    geom_point(aes(color = pressure)) + scale_color_viridis_c()+
    geom_hline(yintercept = 0) +
    geom_hline(yintercept = c(-2, 2), color = "red", linetype = "dashed",
               size = 1) +
    geom_hline(yintercept = c(-4, 4), color = "red", linetype = "dotted",
               size = 1) +
    theme(text = element_text(size = 14))
  
  ggplot(model_4a.pred, aes(I(pressure-1012), devres)) +
    geom_point() +
    facet_wrap(~ location) +
    geom_point(aes(color = temp)) + scale_color_viridis_c()+
    geom_hline(yintercept = 0) +
    geom_hline(yintercept = c(-2, 2), color = "red", linetype = "dashed",
               size = 1) +
    geom_hline(yintercept = c(-4, 4), color = "red", linetype = "dotted",
               size = 1) +
    theme(text = element_text(size = 14))
  
  
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
  
  ggplot(model_4a.pred, aes(xbeta, devres, color = as.factor(lowrain))) +
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
  
  ggplot(model_4a.pred, aes( I(pressure - 1012), devres, color = as.factor(lowrain))) +
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

# compare values in a table in overleaf


#### 5b) ####
# ROC-curves####
# Calculate for model 2, 3, and 4.
library(pROC)
(roc.2 <- roc(lowrain ~ p.2, data = pred.phat))
# save the coordinates in a data frame for plotting.
roc.df.2 <- coords(roc.2, transpose = FALSE)
roc.df.2$model <- "2b"
roc.df.2

(roc.3 <- roc(lowrain ~ p.3, data = pred.phat))
# save the coordinates in a data frame for plotting.
roc.df.3 <- coords(roc.3, transpose = FALSE)
roc.df.3$model <- "3a"
head(roc.df.3)

(roc.4 <- roc(lowrain ~ p.4, data = pred.phat))
# save the coordinates in a data frame for plotting.
roc.df.4 <- coords(roc.4, transpose = FALSE)
roc.df.4$model <- "4a"
head(roc.df.4)


# Built-in function for plotting one Roc-curve
# Note that the x-axis is reversed!
# If we want the diagonal with geom_abline, it has to be reversed!
# Since both axes are 0-1, we want a square plot area:
# + coord_fixed()
ggroc(roc.2) +
  geom_abline(intercept = 1, slope = 1, linetype = "dashed") +
  coord_fixed() +
  labs(title = "ROC-curve for model 2b")
  
ggroc(roc.3) +
  geom_abline(intercept = 1, slope = 1, linetype = "dashed") +
  coord_fixed() +
  labs(title = "ROC-curve for model 3a")
  
ggroc(roc.4) +
  geom_abline(intercept = 1, slope = 1, linetype = "dashed") +
  coord_fixed() +
  labs(title = "ROC-curve for model 4a")

# Plot the three ROC-curves:
# Use geom_path() instead of geom_line()
#
# For model 3 the curve is color coded according to
# the threshold. The color scheme is set by
# + scale_color_gradientn(colours = rainbow(5)) +
#
# Note that the x-axis is reversed!
# + scale_x_reverse()
# You could use 1 - spec instead.
# If we want the diagonal with geom_abline, it has to be reversed!
#
# Since both axes are 0-1, we want a square plot area:
# + coord_fixed()
#
ggplot(roc.df.3, aes(specificity, sensitivity)) +
  geom_path(aes(color = threshold), size = 2) +
  geom_path(data = roc.df.ideal, color = "black", size = 1) +
  geom_path(data = roc.df.2, color = "red", size = 1,
            linetype = "dashed") +
  geom_point(data = roc.df.3[I_max.3, ], color = "black", size = 3) +
#  geom_abline(intercept = 1, slope = 1, linetype = "dashed") +
  scale_color_gradientn(colours = rainbow(5)) +
  coord_fixed() +       # square plotting area
  scale_x_reverse() +   # Reverse scale on the x-axis!
  labs(title = "ROC-curve for model 3a",
       caption = "Black dot = optimal threshold") +
  theme(text = element_text(size = 14))

# ROC-curves for all models####
roc.2 <- roc(lowrain ~ p.2, data = pred.phat)
roc.df.2 <- coords(roc.2, transpose = FALSE)
roc.df.2$model <- "2b"
roc.3 <- roc(lowrain ~ p.3, data = pred.phat)
roc.df.3 <- coords(roc.3, transpose = FALSE)
roc.df.3$model <- "3a"
roc.4 <- roc(lowrain ~ p.4, data = pred.phat)
roc.df.4 <- coords(roc.4, transpose = FALSE)
roc.df.4$model <- "4a"

roc.df <- rbind(roc.df.2, roc.df.3, roc.df.4)

# Plot all the curves, in different colors:
ggplot(roc.df, aes(specificity, sensitivity,
                            color = model)) +
  geom_path(size = 1) +
  coord_fixed() +       # square plotting area
  scale_x_reverse() +   # Reverse scale on the x-axis!
  labs(title = "ROC-curves for all the models") +
  theme(text = element_text(size = 14))

# AUC
roc.3
auc(roc.3)
# Confidence interval for AUC
(ci.3 <- ci(roc.4))
# lower limit:
ci.3[1]
# AUC:
ci.3[2]
# upper limit:
ci.3[3]

#Collect AUC and intervals for all the models:
(aucs <- 
  data.frame(
    model = c("2b", "3a", "4a"),
    auc = c(auc(roc.2), auc(roc.3), auc(roc.4)),
    lwr = c(ci(roc.2)[1], ci(roc.3)[1], ci(roc.4)[1]),
    upr = c(ci(auc(roc.2))[3], ci(auc(roc.3))[3], ci(auc(roc.4))[3])))

# Compare the AUC for the models:

roc.test(roc.3, roc.2)
roc.test(roc.4, roc.2)
roc.test(roc.3, roc.4)

# we want a big AUC
# create a table in overleaf with values from above

#### 5c) ####

# Create the data for the Ideal model by hand:
roc.df.ideal <- data.frame(sensitivity = c(0, 1, 1),
                           specificity = c(1, 1, 0),
                           threshold = c(NA, NA, NA))
roc.df.ideal$model <- "ideal"

# experiment with different values of "limit" to find the
# optimal combination of sens and spec.

(row.01 <- table(weather$lowrain))

# -------- MODEL 2b --------------

limit_2 <- 0.299
pred.phat$yhat.2 <- as.numeric(pred.phat$p.2 > limit_2)
(col.01.2 <- table(pred.phat$yhat.2))
(confusion.2 <- table(pred.phat$lowrain, pred.phat$yhat.2))
(spec.2 <- confusion.2[1, 1] / row.01[1])
(sens.2 <- confusion.2[2, 2] / row.01[2])
spec.2 - sens.2

(accu.2 <- sum(diag(confusion.2)) / sum(confusion.2))
(prec.2 <- confusion.2[2, 2] / col.01.2[2])

                  
# -------- MODEL 3a --------------

limit_3 <- 0.3
pred.phat$yhat.3 <- as.numeric(pred.phat$p.3 > limit_3)

(col.01.3 <- table(pred.phat$yhat.3))
(confusion.3 <- table(pred.phat$lowrain, pred.phat$yhat.3))
(spec.3 <- confusion.3[1, 1] / row.01[1])
(sens.3 <- confusion.3[2, 2] / row.01[2])
abs(spec.3 - sens.3)

(accu.3 <- sum(diag(confusion.3)) / sum(confusion.3))
(prec.3 <- confusion.3[2, 2] / col.01.3[2])

                   
# -------- MODEL 4a --------------

limit_4 <- 0.285
pred.phat$yhat.4 <- as.numeric(pred.phat$p.4 > limit_4)
(col.01.4 <- table(pred.phat$yhat.4))
(confusion.4 <- table(pred.phat$lowrain, pred.phat$yhat.4))
(spec.4 <- confusion.4[1, 1] / row.01[1])
(sens.4 <- confusion.4[2, 2] / row.01[2])
spec.4 - sens.4

(accu.4 <- sum(diag(confusion.4)) / sum(confusion.4))
(prec.4 <- confusion.4[2, 2] / col.01.3[2])

# put all values in a table in overleaf and compare. Probably acc and prec got shitty


#### 5d) ####
# Hosmer-Lemeshow-test####
library(ResourceSelection)
# plot in sorted p-order
# order(variable) gives the ranks for the values in variable.
# It can then be used to sort the data frame:
pred.sort <- pred.phat[order(pred.phat$p.3), ]
pred.sort$rank <- seq(1, nrow(pred.sort))
head(pred.sort)

# Divide the n=500 observations into g=10 groups:
n <- nrow(pred.sort)
g <- 10
# with ng = 50 observations each:
ng <- n/g

# Plot p_i and Y_i
# Add i vertical jitter to Y_i to separate them
ggplot(pred.sort, aes(rank, p.3)) +
  geom_point() +
  geom_jitter(aes(y = lowrain), height = 0.01) +
  geom_vline(xintercept = seq(ng, nrow(pred.sort) - ng, ng)) +
  labs(title = "Model 3: Estimated probabilities by increasing size",
       caption = "g = 10 groups",
       x = "(i) = 1,...,n", y = "p-hat") +
  theme(text = element_text(size = 14))

# HL by hand
# The following can be done using the output of the
# hoslem.test function, see below.
# I do is here to illustrate the steps.

# A for-loop to set the group numbers:
pred.sort$group <- NA
for (k in seq(1, g)) {
  I <- (k - 1)*ng + seq(1, ng)
  pred.sort$group[I] <- k
}
head(pred.sort)

# Calculate Observed and Expected in each group:
# aggregate(y ~ x, FUN = mean) calculates the mean of y
# separately for each group.
# merge(data1, data2) joins two data frames using any common
# variables as keys, in this case, "group".

# Number of successes:
OE1 <- merge(aggregate(lowrain ~ group, data = pred.sort, FUN = sum),
             aggregate(p.3 ~ group, data = pred.sort, FUN = sum))
OE1
# Number of failures = n_g - successes:
OE0 <- OE1
OE0$lowrain <- ng - OE1$lowrain
OE0$p.3 <- ng - OE1$p.3
# A variable to cure for color coding:
OE1$outcome <- "O1k, E1k"
OE0$outcome <- "O0k, E0k"
# Bind the two data sets as rows (r):
(OE <- rbind(OE1, OE0))

# And plot:
# Set the tickmarks on the x-axis to integers 1,...,g
ggplot(OE, aes(group, p.3, color = outcome)) +
  geom_line(size = 1) +
  geom_line(aes(y = lowrain), linetype = "dashed", size = 1) +
  labs(title = "Model 3: Observed and expected in each group",
       caption = "solid = expected, dashed = observed",
       y = "number of observations") +
  theme(text = element_text(size = 14)) +
  scale_x_continuous(breaks = seq(1, g))

# The test:
(chi2HL <- sum((OE$lowrain - OE$p.3)^2/OE$p.3))
# chi2-quantile to compare with:
qchisq(1 - 0.05, g - 2)
# or P-value:
pchisq(chi2HL, g - 2, lower.tail = FALSE)

# HL using hoslem.test####
# p+1:
length(model_3a$coefficients)
# so we need g > 3
# while the smallest expected value is at least approx 5:
# Allowing 4 here and have experimented with g:
(HL.3 <- hoslem.test(pred.sort$lowrain, pred.sort$p.3, g = 8))
HL.3$expected

# Collect the data in a useful form for plotting:
(HL.df.3 <- data.frame(group = seq(1, 8),
                       Obs0 = HL.3$observed[, 1],
                       Obs1 = HL.3$observed[, 2],
                       Exp0 = HL.3$expected[, 1],
                       Exp1 = HL.3$expected[, 2]))

ggplot(HL.df.3, aes(group, Obs0)) +
  geom_line(linetype = "dashed", color = "red", size = 1) +
  geom_line(aes(y = Obs1), linetype = "dashed", size = 1) +
  geom_line(aes(y = Exp0), color = "red", size = 1) +
  geom_line(aes(y = Exp1), size = 1) +
  labs(title = "Model 3: Observed and expected in each group",
       caption = "solid = expected, dashed = observed, red = 0, black = 1",
       y = "number of observations") +
  scale_x_continuous(breaks = seq(1, 11)) +
  theme(text = element_text(size = 14))
  
# ------------------------ Model 3, short version
# g = 15 is good for mean = 5, p-value = 8.672e-07
# but the plot looks horrible
# g = 9 is good for the plot, but mean = 12.7. p-value = 2.587e-08
group_3 <- 15
length(model_3a$coefficients)
(HL.3 <- hoslem.test(pred.sort$lowrain, pred.sort$p.3, 
                        g = group_3))
HL.3$expected

(HL.df.3 <- data.frame(group = seq(1, group_3),
                     Obs0 = HL.3$observed[, 1],
                     Obs1 = HL.3$observed[, 2],
                     Exp0 = HL.3$expected[, 1],
                     Exp1 = HL.3$expected[, 2]))
# How many observations in each group?
HL.df.3$Obs0 + HL.df.3$Obs1

ggplot(HL.df.3, aes(group, Obs0)) +
  geom_line(linetype = "dashed", color = "red", size = 1) +
  geom_line(aes(y = Obs1), linetype = "dashed", size = 1) +
  geom_line(aes(y = Exp0), color = "red", size = 1) +
  geom_line(aes(y = Exp1), size = 1) +
  labs(title = "Model 3a: Observed and expected in each group",
       caption = "solid = expected, dashed = observed, red = 0, black = 1",
       y = "number of observations") +
  scale_x_continuous(breaks = seq(1, group_3)) +
  theme(text = element_text(size = 14))
  

# ------------------------- Do the same for model 2b:
# g = 28 gives mean = 5.1 and p-value = 0.2973. The plot looks so so.
# g = 10 gives mean = 15.05, p-value = 0.213. The plot looks a lot smoother and still ok.
group_2 <- 28
length(model_2b$coefficients)
(HL.2 <- hoslem.test(pred.sort$lowrain, pred.sort$p.2, 
                        g = group_2))
HL.2$expected

(HL.df.2 <- data.frame(group = seq(1, group_2),
                     Obs0 = HL.2$observed[, 1],
                     Obs1 = HL.2$observed[, 2],
                     Exp0 = HL.2$expected[, 1],
                     Exp1 = HL.2$expected[, 2]))
# How many observations in each group?
HL.df.2$Obs0 + HL.df.2$Obs1

ggplot(HL.df.2, aes(group, Obs0)) +
  geom_line(linetype = "dashed", color = "red", size = 1) +
  geom_line(aes(y = Obs1), linetype = "dashed", size = 1) +
  geom_line(aes(y = Exp0), color = "red", size = 1) +
  geom_line(aes(y = Exp1), size = 1) +
  labs(title = "Model 2b: Observed and expected in each group",
       caption = "solid = expected, dashed = observed, red = 0, black = 1",
       y = "number of observations") +
  scale_x_continuous(breaks = seq(1, group_2)) +
  theme(text = element_text(size = 14))
  

# ------------------------------ And for model 4:
# p-value = 0.1496 => not significantly bad
# g = 8 is best with mean = 4.429233
# g = 14: mean = 1.71, p-value = 0.5978
group_4 <- 8
(HL.4 <- hoslem.test(pred.sort$lowrain, pred.sort$p.4, g = group_4))
HL.4$expected
(HL.df.4 <- data.frame(group = seq(1, group_4),
                       Obs0 = HL.4$observed[, 1],
                       Obs1 = HL.4$observed[, 2],
                       Exp0 = HL.4$expected[, 1],
                       Exp1 = HL.4$expected[, 2]))

ggplot(HL.df.4, aes(group, Obs0)) +
  geom_line(linetype = "dashed", color = "red", size = 1) +
  geom_line(aes(y = Obs1), linetype = "dashed", size = 1) +
  geom_line(aes(y = Exp0), color = "red", size = 1) +
  geom_line(aes(y = Exp1), size = 1) +
  labs(title = "Model 4a: Observed and expected in each group",
       caption = "solid = expected, dashed = observed, red = 0, black = 1",
       y = "number of observations") +
  scale_x_continuous(breaks = seq(1, group_4)) +
  theme(text = element_text(size = 14))


# ----
# the mean should be around 5 and p < 0.5. the lines in the plot should be close to each other
# for overleaf: present two different group values that give obviously different results
