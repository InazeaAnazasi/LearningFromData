#---------------- Learning  from Data: Assignment 2 ----------------------------
#
# from 31st of September to 7th of November, Lukas Schmid
#


#----------------- Preliminaries ----
library(readr)
library(magrittr)
library(purrr)
library(dplyr)
library(tidyr)
library(ggplot2)
theme_set(theme_bw())
library(matlib) # for dealing with matrices
library(tictoc) # to measure code runtime
library(wesanderson)

data <- read.csv("assignment2wine.csv")

#-----------------  Task 1 ----------------------------------------------------
# importing the data and checking the number of dimensions
## number of dimensions - one variable is not in the X-Matrix,
## that's where the -1 comes from. In the end, every x has an element x_0 = 0,
## so the final dimensionality of X must be (d+1)*(N)
dim(data)
d <- ncol(data) -1 
N <- nrow(data)
X_dimensionality <- c(N, d+1) # element to compare the actual X and w with later on

# extracting the variables and matrices
## let x(n) be the vector of the explanatory variables at the nth observation, 
## with dimensionality d+1 (remember that x_0 = 1):
## so X is the matrix of all x(n) with N rows, where every row n consists of 
## the transpose of x(n):
data$x0 = rep(1, N) # add the x_0
X <- as.matrix( data[,c(6, 1, 2, 3, 4)])
## Y is the dependent variable:
Y <- as.matrix(data[,5])
## checking the dimensionality of X
dim(X) == X_dimensionality

# calculating w in the closed form
## create inverses of X and check the dimensionalities
## expecting 5x4898
dim(t(X))
## multiplying X and X^T and checking dimensionalit
## expecting 5*5
dim(t(X) %*% X)
## creating the inverse of the matrix product and checking dimensionality
## expecting 5x5
dim(matlib::inv(t(X) %*% X))
## defining the pseudo inverse
pseudo_inv <- function(X) {
  return(matlib::inv(t(X) %*% X) %*% t(X))
}
## applying the pseudo-inverse to get w
w_lin = pseudo_inv(X) %*% Y
## checking the dimensionality of w - expecting 5x1
dim(w_lin)

# calculating w using linear regression - w is the vector of all coefficients
model <- lm(alcohol  ~ acidity + residualSugar + density + pH, data)
model$coefficients

# compare the two weight vectors
data.frame(variable = colnames(data[c(6, 1:4)]),
           w_lin = c(w_lin[,1]),
           w_lm = model$coefficients[1:5])

# #---------------- Task 2 -----------------------------------------------------

# write function to get X
create_X <- function(nrow, ncol) { # takes vectors as inputs
  return(matrix(data=c(rep(1, nrow), runif(ncol*nrow, 0, 1)), nrow, ncol+1))
}

# write function to calculate w in closed form, only specifying the number of rows and columns, but measurning runtime
closed_form_w_runtime <- function(nrow, ncol) {
  X = create_X(nrow, ncol)
  Y = runif(nrow, 0, 1)
  tictoc::tic(paste0("Calculating the closed form with ", nrow, " rows and ", ncol, " columns"))
  w = pseudo_inv(X) %*% Y
  runtime = tictoc::toc(quiet=T)
  return(runtime$toc - runtime$tic)
}

# compare the runtimes with different number of rows and columns
plotting_runtimes <- function(nrow_start=10, nrow_by, length, ncol) {
  ## same number of columns, increasing number of rows
  nrow = seq(nrow_start, by=nrow_by, length.out = length)
  ncol = rep(ncol, length)
  ## creating an object with the runtimes
  runtimes <- map2_dbl(.x=nrow, .y=ncol,
                       .f=function(x, y) closed_form_w_runtime(x, y))
  ## prepare a tibble to plot from
  return(
    tibble(ncol = ncol, nrow = nrow,
                 runtimes = runtimes))
}

# binding together data of different runtime tries
tic()
data <- bind_rows(
  plotting_runtimes(nrow_start=50, nrow_by=500, length=100, ncol=2),
  plotting_runtimes(nrow_start=50, nrow_by=500, length=100, ncol=5),
  plotting_runtimes(nrow_start=50, nrow_by=500, length=100, ncol=10),
  plotting_runtimes(nrow_start=50, nrow_by=500, length=100, ncol=15),
  plotting_runtimes(nrow_start=50, nrow_by=500, length=100, ncol=20))
toc()

# plotting the data
plot <- ggplot(data,
       aes(x=nrow, y=runtimes, group=ncol, col=factor(ncol))) +
  geom_point(alpha=0.6) +
  geom_smooth(method="glm")

plot +
  scale_x_continuous(name="number of rows in matrix", 
                     labels=scales::number_format(big.mark=",")) +
  scale_y_continuous(name="runtime for calculation of closed form (seconds)") +
  scale_color_manual(values=wes_palette("Zissou1", type = "discrete"),
                     name="number of\ncolums\nin matrix")

# #---------------- Task 3 -----------------------------------------------------
# Task 3a: Implement Windrow-Hoff in batch mode----

X <- matrix(c(data$residualSugar, data$density), ncol=2) 
# create X from the two columns of the data
Y <- matrix(data$alcohol, ncol=1)

windrow_hoff_batch <- function(X, Y, eta) { # X and Y have to be matrices
  N <- nrow(X) 
  # number of observations in X
  d <- ncol(X) 
  # dimensionality of X
  X <- cbind(rep(1, N), X) 
  # adding the column x0 to x; we now have a "complete" X to use in the formula
  w <- runif(d+1, 0, 1) 
  # creating w(t=0) with random start values
  return(w - eta * t(X) %*% (X %*% w - Y)) 
  # this is essentially the Windrow-Hoff in batch mode
}

# Task 3b: Implement Windrow-Hoff in stochastic mode
# (for stochastic gradient descent) ----

# function: generate D for wh_sto_update() from data and the variable names
wh_generate_D <- function(data, X, Y) { # X is a vector of variable names (all in strings), Y is a single variable name (string)
  # choose X by using variable names and add a new column x0 to X
  X <- cbind(x0=rep(1, nrow(data)), data[X])
  # create a list of vectors from X
  X_vectors <- pmap(X, ~c(...))
  # generate a new dataframe from vectorised X and Y
  return(
    data.frame(x = cbind(X_vectors),
               y = data[[Y]]))# cbind(X_vectors) is needed because data.frame() doesn't know how to bind a list and a vector together
}

# function: create w(t+1) from D (generated by wh_generate_D), eta and w(t) 
wh_sto_update <- function(D, eta, w) { # D has to be generated by wh_generate_D
  # randomly choose x_n and y_n
  n <- sample(1:nrow(D), 1)
  x_n <- D[["X_vectors"]][[n]]
  y_n <- D[["y"]][n]
  # update w from old w, eta, x_n and y_n
  return(w - (eta * (x_n %*% (t(w) %*% x_n - y_n)))) # additional brackets only serve to support reading the formula
}

# manually trying to calculate a bunch of w's
# W <- list(c(1, 1, 1))
# for (i in 2:100) {
#   W[[i]] <- wh_sto_update(D, 0.0001, W[[i-1]])
# }
 
# function: let wh_sto_update run T times and store all w(t) in a list
wh_sto <- function(D, eta, w0, times) {
  W <- list(w0)
  for (t in 1:times) {
    W[[t+1]] <- wh_sto_update(D, eta, W[[t]])
  }
  return(W)
}

# function: for a given w, calculate the loss function with data D
wh_loss <- function(D, w) {
  loss_function <- function(x, y, w) {
    as.numeric((y - t(w) %*% x)^2)
  }
  loss <- list()
  for (row in 1:nrow(D)) {
    loss[row] <-loss_function(D$X_vectors[[row]], D$y[[row]], w)
  }
  return(sum(as.numeric(loss)))
}

# function: with given data, variables, eta and w0, calculate the loss function and the list of W of times iterations
wh_batch <- function(data, X, Y, eta, w0, times) {
  D <- wh_generate_D(data, X, Y)
  W <- wh_sto(D, eta, w0, times)# generate weight by updating w times times and store the resulting list of w in W
  Loss <- map(W, ~wh_loss(D, .x)) # for every w in W, calculate the loss function and return it
  return(list(W, Loss))
}

results <- wh_batch(data, c("residualSugar", "density"), "alcohol",
                    0.0001, c(0.1, 0.1, 0.1), 100)

# save the list of losses in a dataframe
Loss_plot_data <- data.frame(
  index=1:length(results[[1]]),
  loss=as.numeric(results[[2]])
)

# visualise the loss function in a contour-plot
ggplot(Loss_plot_data) +
  geom_line(aes(x=index, y=loss))

# # alternatively: plot the weight vectors
# # make a data.frame from the weight vectors
# W_plot_data <- data.frame(
#   index = 1:length(W),
#   w0 = map_dbl(W, function(x) x[[1]]),
#   w1 = map_dbl(W, function(x) x[[2]]),
#   w2 = map_dbl(W, function(x) x[[3]])
#   )
# 
# ggplot(W_plot_data) +
#   geom_point(aes(x=w1, y=w2, color=w0))

# normalising the data
normalised_data <- data %>% 
  mutate_at(.vars=vars(-alcohol), .funs=~(.-mean(.))/sd(.))

results_normal <- wh_batch(normalised_data, c("residualSugar", "density"),
                    "alcohol",
                    0.0001, c(0.1, 0.1, 0.1), 100)

# save the list of losses for data and normalised data in one dataframe
Loss_plot_data <- data.frame(
  index = 1:length(results[[1]]),
  loss = as.numeric(results[[2]]),
  loss_normal = as.numeric(results_normal[[2]])) %>% 
  pivot_longer(cols=c("loss", "loss_normal"))

# visualise the loss function in a contour-plot
ggplot(Loss_plot_data) +
  geom_line(aes(x=index, y=value, col=name, group=name)) +
  scale_x_continuous(name="Iteration\n") +
  scale_y_continuous(breaks=NULL, name="Loss function")+
  scale_color_discrete(name=NULL, labels=c("Data", "Normalised\nData")) +
  labs(title="Loss function over 100 iterations of the wine data\nwith unchanged data and normalised data",
       caption=expression("Dependent variable: alcohol\nIndependent variables: residualSugar, density")) +
  theme()
