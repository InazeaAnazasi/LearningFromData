# --------------- Learning from Data: Assignment 3, Task 4 and 5 --------------#
# 
# 
# ------------------------ Lukas Schmid
#
# -----------------------------------------------------------------------------#

# ------- Loading Packages -----------------------------------------------------
library(ggplot2)
theme_set(theme_minimal())
library(wesanderson)
pal <- wes_palette("Zissou1", 100, type = "continuous")
library(plotly)
library(tibble)
library(magrittr)
# -----------------------------------------------------------------------------#

# ------- Task 4 ---------------------------------------------------------------
# Simulate data for the classification case (2 dimensional input space, binary
# target, not fully linearly separable). Implement the logistic regression
# algorithm using gradient descent (discussed in class).

# prepare functions to generate the data ######################################-
# function that takes a desired line as input and returns a w
getW <- function(slope, intercept, scale=1) {
  return(c((-intercept * scale), (-slope * scale), scale))
}

sigmoid <- function(x) {
  return(exp(x)/(1+exp(x)))
}

# generate data and visualise it ##############################################-
trueW <- getW(0, 0.5)

generateData <- function(N, trueW, min, max) {
  data <- matrix(c(rep(1, N), runif(2*N, min, max)), nrow=N)
  riskScores <- data %*% trueW
  probs <- sigmoid(riskScores)
  cbind(data, riskScores, probs)
}

data <- generateData(1000, trueW, -5, 5)

sample(x = 0:1, size = 1, prob = c(data[1,5], 1-data[1,5]), replace=T) 
sample(x = matrix(c(rep(0, 1000), rep(1, 1000)), ncol=2),
       size = 1000, prob = c(1- data[,5], data[,5]), replace=T) 

plot_ly(x = data[,2], y = data[,3], z = data[,4], color = data[,4],
        type="scatter3d", mode="markers")
plot_ly(x = data[,2], y = data[,3], z = data[,5], color = data[,5],
        type="scatter3d", mode="markers")


