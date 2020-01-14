# ------------------------ Learning from Data: Lecture 10 ---------------------#
#
# Understanding by visualising how a simple transformation of data can lead
# to the classification of more complex patterns by a simple PLA.
# 
# Lukas Schmid
#
# -----------------------------------------------------------------------------#

library(tidyverse)
theme_set(theme_minimal())

## get the points on the lines (with small, but even distances between any two
## subsequent points)
# set the grid size to include in the plot; integer is the distance to the
# origin in every cartesian directions
grid_size <- 5
# set the point density in 1000 points per unit distance from the origin
point_density <- 10
# set the line density in lines that are plotted
line_number <- 10
# get points on the lines (one vector only, can be duplicated to fit multiple
# lines)
pOnLine <- ((-grid_size*point_density):(grid_size*point_density))/point_density
# get the positions of the line (coordinates perpendicular to the lines)
linePos <- seq(-grid_size, grid_size, length.out = line_number)

# put the values directily into a tibble
data <- tibble(PointsOnLine = rep(pOnLine, times = line_number),
               LinePositions = rep(linePos, each = grid_size * 2 * point_density + 1)) %>% 
  transmute(x = PointsOnLine, y = LinePositions,
            x_transform = x^2, y_transform = y^2, colour_x = x, colour_y = y) %>% 
  pivot_longer(cols = -c(colour_x, colour_y)) %>% 
  mutate(transform = ifelse(str_detect(name, "transform"), T, F),
         coordinate = str_sub(name, 0, 1)) %>% 
  select(-name) %>% 
  pivot_wider(names_from = coordinate, values_fn = list(value = list)) %>% 
  unnest(cols = c(x, y))

# plot the whole thing
ggplot(data, aes(y, x, colour = colour_y)) +
  geom_point(alpha=0.5) +
  facet_wrap('transform', scales = "free") +
  scale_colour_viridis_c()

## get the points on a circle
# set the radius of the circle
radius <- 1
# set the number of points on the circle
point_number <- 100
# get a vector with the angles 
angles <- seq(0, 2*pi, length.out = point_number)
# get the x- and y-coordinates from the angles
vector_cos <- Vectorize(cos)
vector_sin <- Vectorize(sin)
x <- vector_cos(angles)
y <- vector_sin(angles)
# put it all together in a tibble
data <- tibble(x, y, angle = angles) %>% 
  mutate(x_transform = x^2, y_transform = y^2, colour_x = x, colour_y = y) %>% 
  pivot_longer(cols = -c(colour_x, colour_y)) %>% 
  mutate(transform = ifelse(str_detect(name, "transform"), T, F),
         coordinate = str_sub(name, 0, 1)) %>% 
  select(-name) %>% 
  pivot_wider(names_from = coordinate, values_fn = list(value = list)) %>% 
  unnest(cols = c(x, y))

ggplot(data, aes(y, x, colour = colour_y)) +
  geom_point(alpha=0.5) +
  facet_wrap('transform', scales = "free") +
  scale_colour_viridis_c()

# -----------------------------------------------------------------------------#
# write function to show how a certain w is being altered by a transformation
# (only 2D -> 2D)
# -----------------------------------------------------------------------------#

w_transformed <- function(w,
                          w_length = 2,
                          point_density = 10) {
  ## draw points that are on w, i.e. that are on the edge of the groups
  # get the function of the line described by w
    # w * x = 0 <-> 
    # w0 + w1*x1 + w2*x2 = 0 <->
    # w0 + w1*x + w2*y = 0 <->
    # y = -w0/w2 - (w1/w2)*x ~~ f(x) = a + bx
  intercept = -w[1]/w[3]
  slope = -w[2]/w[3]
  # get the x- and y-coordinates of the points in w
  x_w <- seq(0, 4, length.out = 4*point_density +1)
  y_w <- intercept + slope * x_w
  # delete all points that have at least one negative coordinate; w already
  # operates in the transformed space, which does not know negative values
  # since it has been squared
  x_w <- x_w[y_w >= 0]
  y_w <- y_w[y_w >= 0]
  # generate all possible combinations from original x- and y-values: any one
  # of the coordinates may have been negative prior to transformation; 
  # accordingly, the number of data points is quadruppled
  x_origin <- rep(c(sqrt(x_w),-sqrt(x_w)), each = 2)
  y_origin <- rep(c(sqrt(y_w),-sqrt(y_w)), times = 2)
  data <- tibble(x_w, y_w) %>% 
    mutate(x1 = sqrt(x_w), x2 = sqrt(x_w), x3 = -sqrt(x_w), x4 = -sqrt(x_w),
           y1 = sqrt(y_w), y2 = -sqrt(y_w), y3 = sqrt(y_w), y4 = -sqrt(y_w),
           color = 1:nrow(.)) %>% 
    pivot_longer(cols = c(x_w : y4)) %>% 
    mutate(coordinate = str_sub(name, 0, 1),
           transformed = ifelse(str_detect(name, "w"), T, F)) %>% 
    select(-name) %>% 
    pivot_wider(names_from = coordinate, values_fn = list(value = list)) %>% 
    unnest(cols = c(x, y))
  
  ggplot(data, aes(x, y, color = color)) +
    geom_point() +
    facet_wrap('transformed', scales = "free") +
    scale_colour_viridis_c()
}

w_transformed(c(-1, 1, 1))
w_transformed(c(1, -1, -1))
w_transformed(c(1, -1, -2))
w_transformed(c(1, 1, -1))

