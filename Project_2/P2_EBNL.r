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

#                   2.5 %      97.5 %
# (Intercept) -0.73897322 -0.43949978
# temp        -0.09218264 -0.05599002
# so temp is significant
exp(beta_est_2b)/(1+exp(beta_est_2b)) # (Intercept) 0.3569551 ; temp 0.4815429
exp(beta_1) # 0.9288 => odds decrease by 7% if temp increases by 1°C
exp(beta_1)^(-1) # 1.076658 => odds increase by 7% if temp decreases by 1°C

#### 2c) ####
x <- c(-10,-9,9,10)
w.x0 = data.frame(temp = x)
pred_2c <- cbind(w.x0, exp(predict(model_2b, w.x0, interval = "confidence")))
# 1  -10   1.1618504
# 2   -9   1.0791266
# 3    9   0.2855435
# 4   10   0.2652129
xpred <- pred_2c[,2]
#beta_est_2b*x - 1.96*

w.pred <- cbind(
  weather,
  phat = predict(model_2b, type = "response"))

w.pred <- cbind(
  w.pred,
  logit = predict(model_2b, se.fit = TRUE))
  
(lambda <- qnorm(1 - 0.05/2))
w.pred$logit.lwr <- w.pred$logit.fit - lambda*w.pred$logit.se.fit
w.pred$logit.upr <- w.pred$logit.fit + lambda*w.pred$logit.se.fit
w.pred$odds.lwr <- exp(w.pred$logit.lwr)
w.pred$odds.upr <- exp(w.pred$logit.upr)
head(w.pred)
# how can we do this for our x temperatures?
