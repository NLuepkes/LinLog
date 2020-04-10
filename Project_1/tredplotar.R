# experiment med 3d-plottar

library(plotly)
library(reshape2)
data(iris)

head(iris)

hmark = c("circle-open", "square", "cross")[iris$Species]
plot_ly(iris,
        x = ~Sepal.Length,
        y = ~Sepal.Width,
        z = ~Petal.Length,
        color = ~Species,
        type = "scatter3d",
        mode = "markers",
        marker = list(color = "black",
                      symbol = hmark,
                      size = 4)
        ) %>%
  layout(legend = list(
    title = list(text = '<b>Species</b>')),
    scene = list(
      xaxis = list(title = 'Sepal length'),
      yaxis = list(title = 'Sepal width'),
      zaxis = list(title = 'Petal length')
    )
  )

hcolors = c("red","blue","green")[iris$Species]
p.iris <- 
  plot_ly(iris, 
          x = ~Sepal.Length, 
          y = ~Sepal.Width, 
          z = ~Petal.Length,
          text = "Species", 
          type = "scatter3d", 
          mode = "markers",
          marker = list(color = hcolors, 
                        size = 4)) %>%
  layout(scene = 
           list(
             xaxis = list(title = 'Sepal length'),
             yaxis = list(title = 'Sepal width'),
             zaxis = list(title = 'Petal length')
           )
  )
p.iris

# one plane####

(petal_lm <- lm(Petal.Length ~ Sepal.Length + Sepal.Width,
                data = iris))
(axis_x <- seq(min(iris$Sepal.Length), 
               max(iris$Sepal.Length), 
               length = 10))
(axis_y <- seq(min(iris$Sepal.Width), 
               max(iris$Sepal.Width), 
               length = 10))

petal_surfs <- expand.grid(Sepal.Length = axis_x,
                           Sepal.Width = axis_y,
                           KEEP.OUT.ATTRS = F)
petal_surfs <- 
  cbind(petal_surfs, 
        fit = predict(petal_lm, petal_surfs),
        conf = predict(petal_lm, 
                       newdata = petal_surfs,
                       interval = "confidence"),
        pred = predict(petal_lm,
                       newdata = petal_surfs,
                       interval = "prediction"))
head(petal_surfs)
petal_surfs$conf.fit <- petal_surfs$pred.fit <- NULL

petal_lm_fit <- acast(petal_surfs, 
                      Sepal.Width ~ Sepal.Length, 
                      value.var = "fit")
petal_lm_conflwr <- acast(petal_surfs, 
                          Sepal.Width ~ Sepal.Length, 
                          value.var = "conf.lwr")
petal_lm_confupr <- acast(petal_surfs, 
                          Sepal.Width ~ Sepal.Length, 
                          value.var = "conf.upr")
petal_lm_predlwr <- acast(petal_surfs, 
                          Sepal.Width ~ Sepal.Length, 
                          value.var = "pred.lwr")
petal_lm_predupr <- acast(petal_surfs, 
                          Sepal.Width ~ Sepal.Length, 
                          value.var = "pred.upr")

color <- rep(0, length(petal_lm_fit))
dim(color) <- dim(petal_lm_fit)
color2 <- color + .5
color3 <- color2 + .5

p0 <- plot_ly(colors = c("green", "green"),
              showscale = FALSE) %>%
  add_trace(x = ~axis_x,
            y = ~axis_y,
            z = ~petal_lm_fit,
            type = "surface",
            opacity = 0.5,
            name = "fitted plane",
            surfacecolor = color,
            cauto = F,
            cmax = 1,
            cmin = 0)
p0 %>%
  add_trace(data = iris,
          x = ~Sepal.Length,
          y = ~Sepal.Width,
          z = ~Petal.Length,
          name = ~Species,
          #            name = "observations",
          type = "scatter3d",
          color = ~Species,
          mode = "markers",
          inherit = FALSE,
          marker = list(color = "black", 
                        symbol = hmark,
                        size = 3)) %>%
  layout(
    legend = list(title = list(text = '<b>Species</b>')),
    scene = list(
      xaxis = list(title = 'Sepal length'),
      yaxis = list(title = 'Sepal width'),
      zaxis = list(title = 'Petal length')
    )
  )

# plane with intervals####

p1 <- plot_ly(colors = c("green", "red"),
              showscale = FALSE) %>%
  add_trace(x = ~axis_x,
            y = ~axis_y,
            z = ~petal_lm_fit,
            type = "surface",
            opacity = 0.5,
            name = "fitted plane",
            surfacecolor = color,
            cauto = F,
            cmax = 1,
            cmin = 0) %>%
  add_trace(x = ~axis_x,
            y = ~axis_y,
            z = ~petal_lm_conflwr,
            type = "surface",
            opacity = 0.5,
            name = "confidence interval",
            surfacecolor = color2,
            cauto = F,
            cmax = 1,
            cmin = 0) %>%
  add_trace(x = ~axis_x,
              y = ~axis_y,
              z = ~petal_lm_confupr,
            type = "surface",
            name = "confidence interval",
              opacity = 0.5,
              surfacecolor = color2,
              cauto = F,
              cmax = 1,
              cmin = 0) %>%
  add_trace(x = ~axis_x,
              y = ~axis_y,
              z = ~petal_lm_predlwr,
              type = "surface",
              name = "prediction interval",
              opacity = 0.5,
              surfacecolor = color3,
              cauto = F,
              cmax = 1,
              cmin = 0) %>%
  add_trace(x = ~axis_x,
              y = ~axis_y,
              z = ~petal_lm_predupr,
              type = "surface",
              name = "prediction interval",
              opacity = 0.5,
              surfacecolor = color3,
              cauto = F,
              cmax = 1,
              cmin = 0) %>%
  add_trace(data = iris,
            x = ~Sepal.Length,
            y = ~Sepal.Width,
            z = ~Petal.Length,
            name = ~Species,
#            name = "observations",
            type = "scatter3d",
            color = ~Species,
            mode = "markers",
            inherit = FALSE,
            marker = list(color = "black", 
                          symbol = hmark,
                          size = 3))
p1
p1 %>%
  layout(
    legend = list(title = list(text = '<b>Species</b>')),
    scene = list(
             xaxis = list(title = 'Sepal length'),
             yaxis = list(title = 'Sepal width'),
             zaxis = list(title = 'Petal length')
             )
         )

