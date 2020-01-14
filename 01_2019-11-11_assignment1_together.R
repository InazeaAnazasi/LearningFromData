#------ Practice about R in Learning from Data --------------------------------
library(tidyr)
library(dplyr)
library(ggplot2)
library(beepr)

#------- Generate Data (1st assignment, 1st exercise) -------------------------
# Simulate data for two classes in a 2D feature space, classes should be 
# linearly separable.
# Generate data in such a way, that there is a gap between the two classes.

# generate a tibble
class(tibble(x=c(1, 3), daniele= "modelleisenbahn", y=2))

# generate a set of random numbers
rnorm(100000, 0, 1) # generate a normal distribution
runif(1000, 0, 1) # genearte a uniform distribution

tibble(x = runif(100000, 0, 1)) %>% # with a pipe
  ggplot(aes(x=x)) +
  geom_histogram(bins=100)
  
object <- tibble(x = rnorm(100000, 0, 1)) # without a pipe
ggplot(object, aes(x=x)) +
  geom_histogram()

# generate a tibble with random number
data <- tibble(x1=runif(100, 0, 1),
               x2=runif(100, 0, 1))
ggplot(data, aes(x=x1, y=x2)) +
  geom_point() # this data does not seem separable!
# function of the line separating: f(x)=-x+1, f: y -> -x+1
# what does that mean for x1 and x2? We want no point to fulfill the equation!
# f(x)=-x+1 <=> x2 = -x1 + 1 <=> x2 + x1 = 1 | no point fulfill the equation!
# x2 + x1 != 1

data <- mutate(data, check = ifelse(x2 + x1 == 1, FALSE, TRUE)) # in tidyverse
data$check = ifelse(data$x2 + data$x1 == 1, FALSE, TRUE) # this is the same
data <- data %>% # this is the same with margrittr and tidyverse
  mutate(x1 = round(x1, 0.5),
         x2 = round(x2, 0.5),
         check = ifelse(x2 + x1 == 1, FALSE, TRUE)) %>% 
  filter(check==TRUE) # only keep those value pairs that are not on the line
# now we have a linearly separable dataset
  

# assign the true y values
data <- data %>% 
  mutate(y_true = ifelse(x1 + x2 > 1, 1, -1))
# see how it turned out
ggplot(data, aes(x=x1, y=x2, color=factor(y_true))) +
  geom_point() +
  geom_abline(intercept = 1, slope= -1) +
  scale_x_continuous(breaks=(1:10)/10) +
  scale_y_continuous(breaks=(1:10)/10)

# now we have a 2-dimensional data with true y-values that is linearly separable
# Implement the perceptron algorithm (PLA) based on
# Christianini & Shawe-Taylor (2000) and run perceptron on simulated data

# step one: update w by using w(k+1) = w(k) + lr * y_true_i * x_i
update_w <- function(w, x_i, y_true_i, lr=1) {
  # update the weight vector with a given observation i in the data
  # and returning a new weight-vector
  w_new <- w + lr * y_true_i * x_i
  return(w_new)
}

# write a function to choose x_i as a vector from the data
choose_x_i <- function(data, i) {
  x1_i <- data$x1[i]  # those two ways are equivalent
  x2_i <- data[i,]$x2 # those two ways are equivalent
  x_i <- c(x1_i, x2_i)
  return(x_i)
}

# chose a true y from the data
choose_y_true_i <- function(data, i) {
  return(data$y_true[i])
}

# update b 
update_b <- function(b, R, y_true_i, lr=1) {
  return(b + y_true_i * R^2 * lr)
}

# find R
find_R <- function(data) {
  norm <- sqrt(data$x1^2 + data$x2^2)
  return(max(norm))
}

# find number of misclassified points given w and b
count_missclassified_points <- function(data, w, b) {
  # create a vector of how w and b classified the given data
  y_classified <- sign(w[1]*data$x1+w[2]*data$x2+b)
  # find those cases where y_true!=y
  return(length(which(data$y_true != y_classified)))
}

# write the code for one iteration
misclassified <- TRUE
W <- list(c(0,0))
B <- list(0)
Errors <- list()
k <- 1
lr <- 0.001
R <- find_R(data)

while(misclassified == TRUE) {
  print(k)
  # count the number of errors made in every iteration k
  Errors[[k]] <- count_missclassified_points(data, W[[k]], B[[k]])
  for(i in 1:nrow(data)) {
    misclassified <-  FALSE
    if(data$y_true[i] *(W[[k]] %*% choose_x_i(data, i) + B[[k]]) <= 0) { 
      # if-clause: check whether that x_i is classified correctly
      y_true_i <- choose_y_true_i(data, i)
      x_i <- choose_x_i(data, i)
      W[[k+1]] <- update_w(W[[k]], x_i, y_true_i, lr)
      B[[k+1]] <- update_b(B[[k]], R, y_true_i, lr)
      misclassified <- TRUE
      k <- k+1
      break
    }
  }
}

# check graphically whether W is the correct vector
ggplot(data, aes(x=x1, y=x2, col=factor(y_true))) +
  geom_point() +
  geom_abline(intercept=-(B[[length(B)]]/W[[length(B)]][2]),
              slope=-(W[[length(B)]][1]/W[[length(B)]][2]))

plot(unlist(Errors), type="line")

tibble(error=unlist(Errors), i=1:length(Errors)) %>% 
  ggplot(aes(y=error, x=i)) +
  geom_line()
