library(ggplot2)

load( file = "RWTH/Master/Erasmus/Vorlesungen/LinLog/R/weather.rda")
summary(weather)

# 1 Linear model

# 1.(a). plot data
ggplot(data = weather, aes(x = temp, y = rain)) +
  geom_point() + 
    labs(title = "weather: temp by rain",
    caption = "Model with no transformations")+
theme(text = element_text(size = 18))

mod.lin <- lm(rain ~ temp, data = weather)
(sum.lin <- summary(mod.lin))
confint(mod.lin)

weather.pred <- cbind(weather,
                 pred = predict(mod.lin, interval = "prediction"))

# Residual analysis
# Residuals vs yhat####
weather.pred$e.lin <- mod.lin$residuals
head(weather.pred)
lim.elin <- max(abs(weather.pred$e.lin)) * c(-1, 1)

ggplot(data = weather.pred, aes(x = pred.fit, y = e.lin)) +
  geom_point() +
  expand_limits(y = lim.elin) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0, color = "orange")
      labs(title = "weather: Residuals ",
    caption = "Model with no transformations")+
theme(text = element_text(size = 18))

# Q-Q-plot and histogram####
ggplot(data = weather.pred, aes(sample = e.lin)) +
  geom_qq() +
  geom_qq_line() +
      labs(title = "weather: QQ plot",
    caption = "Model with no transformations")+
theme(text = element_text(size = 18))

ggplot(data = weather.pred, aes(x = e.lin)) +
  geom_histogram(bins = 20) +
      labs(title = "weather: Historgam",
    caption = "Model with no transformations")+
theme(text = element_text(size = 18))
# this looks horrible

# 1b)
mod.log <- lm(log(rain) ~ temp, data = weather)
weather.pred.log <- cbind(weather,
                 pred = predict(mod.log, interval = "prediction"))

# Residual analysis
# Residuals vs yhat
weather.pred.log$e.lin <- mod.log$residuals
head(weather.pred)
lim.elin <- max(abs(weather.pred.log$e.lin)) * c(-1, 1)

ggplot(data = weather.pred.log, aes(x = pred.fit, y = e.lin)) +
  geom_point() +
  expand_limits(y = lim.elin) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0, color = "orange") +
      labs(title = "weather: temp by log(rain)",
    caption = "Model with transformations")+
theme(text = element_text(size = 18))

# Q-Q-plot and histogram####
ggplot(data = weather.pred.log, aes(sample = e.lin)) +
  geom_qq() +
  geom_qq_line() +
labs(title = "weather: QQ plot",
    caption = "Model with transformations: temp, log(rain)")+
theme(text = element_text(size = 18))
ggplot(data = weather.pred.log, aes(x = e.lin)) +
  geom_histogram(bins = 20) +
  labs(title = "weather: Histogram",
    caption = "Model with transformations: temp, log(rain)")+
theme(text = element_text(size = 18))

# this also looks bad :(
