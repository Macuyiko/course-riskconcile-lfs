library(ggplot2)
library(ggvis)

data <- data.frame(x=1:100, y=runif(100, 0, 10), z=as.integer(runif(100,0,3)))

ggplot(data, aes(x, y)) + geom_point()

# Or as lines instead?
ggplot(data, aes(x, y)) + geom_line()

# Adding statistics
ggplot(data, aes(x, y)) + geom_line() +
  stat_smooth()

# Changing the aesthetics
ggplot(data, aes(x, y, color=z)) + geom_line() + stat_smooth()

# Maybe these are groups instead?
ggplot(data, aes(x, y, color=factor(z))) + stat_smooth()

# Changing the theme
ggplot(data, aes(x, y, color=factor(z))) + stat_smooth() +
  theme_light()
  
# Adding facets
ggplot(data, aes(x, y, group=z)) + stat_smooth() +
  facet_grid(. ~ z) + theme_light()

# Interactivity with ggvis
ggvis(data, ~x, ~y) %>% 
  layer_smooths(se = TRUE,
                span = input_slider(min = 0.3, max = 1, value = 0.8, step = 0.1, label = "Smoothing span")) %>% 
  layer_points(size := 25)
