#### Exercice 1.4 from "Learning from Data" | Task 1 in Assignment from 21/10/2019 ####
# Simulate data for two classes in a 2D feature space, classes should be linearly separable. Generate data in such a way, that there is a gap between the two classes.

library(tidyr)
library(dplyr)
library(ggplot2)
library(gridExtra)
set.seed(20)
theme_set(theme_bw())

# X has two dimensions (d=2)
# initialise data set:
# inputs x_n as random points of data, with each point having two coordinates; adding also the x0:
data <- tibble(index=1:10,
               x0=rep(1,times=10),
               x1=round(runif(10, 0, 1),1),
               x2=round(runif(10, 0, 1), 1)) %>%
  mutate(y_true=ifelse(x1+x2<1, -1, 1)) %>%
  filter(x1+x2!=1)

# visualising the data ----
# this polygon colors the background
polygon <- tibble(x1=c(0,1,1,0,0,1),
                  x2=c(1,0,1,1,0,0),
                  y_true=c(1,1,1,-1,-1,-1))

ggplot(data) +
  geom_point(aes(x1, x2, color=as.factor(y_true)), size=3, shape=1, stroke=2) +
  geom_abline(slope=-1, intercept=1) +
  scale_color_manual(values=c("blue", "red"), name="Predicted y", guide=FALSE) +
  scale_x_continuous(breaks=(1:10)/10, minor_breaks = F, name=NULL) +
  scale_y_continuous(breaks=(1:10)/10, minor_breaks = F, name=NULL) +
  geom_polygon(data=polygon, aes(x1, x2, fill=as.factor(y_true)), alpha=0.3) +
  scale_fill_manual(values=c("blue", "red"), name="True y", guide=FALSE) +
  labs(caption="The line with the intercept 1 and the slope -1 separates the data.")
  
# choosing a random w and let it classify the data ----
weight <- c(0.5,0.5,-1)

# function to calculate how w classifies the data:
classify_w <- function(data, weight) {
  data %>% 
    mutate(y1=ifelse(weight[1]+x1*weight[2]+x2*weight[3]>0, 1, -1)) %>% 
    return()
}

# updating w on one misclassified x ----
# choosing a misclassified x:
find_misclassified <- function(data, weight) {
  data <- classify_w(data, weight)
  return(filter(data, y1!=y_true)$index[1]) # getting the index of the first x that is misclassified
}

# function to visualise how any w classifies the data:
visualise_w <- function(data, weight) {
  data <- classify_w(data, weight)
  x_misclassified <- find_misclassified(data, weight)
  w_vector <- paste0("The weight-vector is (", weight[1], ", ", weight[2], ", ", weight[3], ")")
  ggplot(data) +
    geom_point(aes(x1, x2, color=as.factor(y1)), size=3, shape=1, stroke=2) +
    geom_abline(slope=-1, intercept=1) +
    scale_color_manual(values=c("blue", "red"), name="Predicted y", guide=FALSE) +
    scale_x_continuous(breaks=(1:10)/10, minor_breaks = F, name=NULL) +
    scale_y_continuous(breaks=(1:10)/10, minor_breaks = F, name=NULL) +
    geom_polygon(data=polygon, aes(x1, x2, fill=as.factor(y_true)), alpha=0.3) +
    scale_fill_manual(values=c("blue", "red"), name="True y", guide=FALSE) +
    geom_abline(slope=-weight[2]/weight[3], intercept=-weight[1]/weight[3], linetype="dotted") +
    geom_point(data=data[x_misclassified,], aes(x1, x2)) +
    labs(caption=w_vector)
}

# performing calculation and visualisation of how w classifies the data:
visualise_w(data, weight)

# updating w on a misclassified x:
update_w <- function(data, weight, learn_rate=1) {
  i <- find_misclassified(data, weight)
  return(weight + data$y_true[i]*learn_rate*(c(data$x0[i], data$x1[i], data$x2[i])))
}

# let w again classify the data
weight_vectors <- list(weight)
graphs <- list()

for (i in 1:15) {
  graphs[[i]] <- visualise_w(data, weight_vectors[[i]])
  weight_vectors[[i+1]] <- update_w(data, weight_vectors[[i]], 0.3)
}

grid.arrange(graphs[[1]], graphs[[2]], graphs[[3]], graphs[[4]], graphs[[5]], graphs[[6]], graphs[[7]], graphs[[8]], graphs[[9]], graphs[[10]],graphs[[11]],graphs[[12]],graphs[[13]],graphs[[14]],graphs[[15]],
             nrow=3)
