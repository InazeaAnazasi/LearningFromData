# ---------------------------------------------------------------------------- #
# Learning From Data Assignment 2
# Melania Scerra
# 22.11.2019
# ---------------------------------------------------------------------------- #


#import file
data<-read.csv(file ="assignment2wine.csv")

# ---------------------------------------------------------------------------- #
# Task 1 ####
# ---------------------------------------------------------------------------- #

# prepare matrices for linear regression
y <- data$alcohol # dim 4898 x 1
X <- as.matrix(data[, 1:4]) #dim 4898 x 4
X <- cbind(rep(1,3898),X) #dim 4898 x 5

# calculate weights
library(matlib) #needed to call inv
pseudo_inv <- inv(t(X)%*%X)%*%t(X) #dim 5 x 48984
w = pseudo_inv %*% y # dim 5 x 1

#calculate r solution
R_solution<-lm(alcohol~ acidity+residualSugar+density+pH, data=data)

# ---------------------------------------------------------------------------- #
# Task 2 ####
# ---------------------------------------------------------------------------- #

# Goal:  explore the dependency of the number of samples 
# and the number of independent variables on the computation time
# (basically the dimensions of X)

# result: it is linear with the number of data and quadratic 
# with the number of features

# Changing the number of rows
# setting the dimensions for the matrix X. 
ncols <- 3
nrowVector<-2*ncols #initialize vector of number of rows

# initializing the vectors
Duration<- 0  # this is the duration calculated at each iteration
AverageDuration<-Duration # this is the average duration

# 1st loop - varying the number of rows 100 times
for (i in 1:100){
  #nrow should be > ncols. The choice of 2 as a row-to-column ratio for X is
  # arbitrary
  nrows <- 2*ncols*i*100
  n <-100
  
  # 2nd loop - To calculate the average duration, run the following code n times
  for (i in 1:n){
    #Generating data from a uniform distribution
    y<- runif(nrows,min = 0, max = 50)
    X<-matrix(data = runif(n = ncols*nrows, min = 2, max = 10), ncol = ncols, nrow = nrows, byrow = TRUE)
    
    # Run Sys.time() before code execution
    Time1 <- Sys.time()
    pseudo_inv<-inv(t(X)%*%X)%*%t(X) 
    w = pseudo_inv %*% y 
    # Run Sys.time() after code execution
    Time2<-Sys.time()
    # create a vector with the time elapsed at each iteration
    Duration<-c(Duration,Time2 - Time1)
    
  }
  # Calculate the average duration for given dimensions
  AverageDuration<-c(AverageDuration, mean(Duration))
  # Keep track of the values for nrows
  nrowVector<-c(nrowVector, nrows)
}
plot(AverageDuration~nrowVector)


ncolVector<-0 #initialize vector of number of columns

# initializing the vectors
Duration<- 0  # this is the duration calculated at each iteration
AverageDuration<-Duration #this is the mean duration
nrows<-100 #set nrows large enough to ensure invertibility
#1st loop - varying the number of columns 100 times
for (i in 1:3){
  #nrow should be > ncols. The choice of 2 as a row-to-column ratio for X is
  # arbitrary
  
  ncols <- 0.1*i*100 
  n <-100
  
  #2nd loop - To calculate the average duration, run the following code n times
  for (i in 1:n){
    #Generating data from a uniform distribution
    y<- runif(nrows,min = 0, max = 50)
    X<-matrix(data = runif(n = ncols*nrows, min = 2, max = 10), ncol = ncols, nrow = nrows, byrow = TRUE)
    
    #Run Sys.time() before code execution
    Time1 <- Sys.time()
    pseudo_inv<-inv(t(X)%*%X)%*%t(X) 
    w = pseudo_inv %*% y 
    #Run Sys.time() after code execution
    Time2<-Sys.time()
    #create a vector with the time elapsed at each iteration
    Duration<-c(Duration,Time2 - Time1)
    
  }
  #Calculate the average duration for given dimensions
  AverageDuration<-c(AverageDuration, mean(Duration))
  #Keep track of the values for nrows
  ncolVector<-c(ncolVector, ncols)
}
plot(AverageDuration~ncolVector)

# ---------------------------------------------------------------------------- #
# Task 3 - Online Widrow Hoff####
# ---------------------------------------------------------------------------- #

data<-read.csv(file ="assignment2wine.csv")
y <-data$alcohol # dim 4898 x 1
X<- as.matrix(data[,c(2,4)]) #dim 4898 x 2
X<-cbind(rep(1,4898),X) #dim 4898 x 3

#Set learning rate eta
LearningRate <- 0.01
#initialize vector of weights
w<-rep(0,3)

#Define loss function 
CostFunction <- function(w,X,y){
  sum( (w %*% t(X) - y)^2 )
}

#you could shuffle the training set to avoid cycles
NRuns<-10
for(j in 1:NRuns){
  Loss<-0
  for(i in 1:length(y)){
    w <- w - LearningRate*( w%*%(X[i,])-y[i])*X[i,] 
  }
  Loss<-CostFunction(w,X,y)
}

# Plot loss function --------------------------------------------------------- #

library(plotly)
# Create a sequence of incrementally increasing values for both w1 and w2
#w1grid <-  seq(-10,10, 0.5)
w2grid <-  seq(-100,100, 10)
w3grid <-  seq(-100,100, 10)

# Generate a dataframe with every possible combination of w2, w3
data.fit <-  expand.grid(w2 = w2grid, w3 = w3grid)


#Feed the dataframe in the loss function
w1 <- 10  #w1 fixed
CostVector<-c()
for(w2 in w2grid){
  for(w3 in w3grid){
    w <- c(w1,w2,w3)
    Cost<-CostFunction(w,X,y)
    CostVector<-append(CostVector,Cost)
  }
}

#The following matrix ha w2 as columns and w1 as rows
CostMatrix <- matrix(CostVector,ncol=length(w3grid),byrow=TRUE)

plot_ly(
  x = w2grid,
  y= w3grid,
  z = ~CostMatrix, 
  type = "contour")


# Normalizing features
means <-colMeans(X[,2:3])
sd <-apply(X[,2:3], 2, sd)

XMinusMean<- sweep(X[,2:3], 2, means,FUN="-")
Xnorm <-cbind(rep(1,4898),sweep(XMinusMean,2,sd,FUN = "/"))
#now the features have mean = 0 and sd = 1

#plotting:
w2gridNorm <-  seq(-5,5, 0.5)
w3gridNorm <-  seq(-5,5, 0.5)


#Calculate cost at each point of the grid
w1 <- 10  #w1 fixed
CostVector<-c()
for(w2 in w2gridNorm){
  for(w3 in w3gridNorm){
    w <- c(w1,w2,w3)
    Cost<-CostFunction(w,Xnorm,y)
    CostVector<-append(CostVector,Cost)
  }
}

#The following matrix ha w2 as columns and w1 as rows
CostMatrixNorm <- matrix(CostVector,ncol=length(w3grid),byrow=TRUE)

plot_ly(
  x = w2gridNorm,
  y= w3gridNorm,
  z = ~CostMatrixNorm, 
  type = "contour")
#https://rdrr.io/rforge/wordspace/man/normalize_rows.html


# GD Batch mode -------------------------------------------------------------- #

data<-read.csv(file ="assignment2wine.csv")
y <-data$alcohol # dim 4898 x 1
X<- as.matrix(data[,c(2,4)]) #dim 4898 x 2
X<-cbind(rep(1,4898),X) #dim 4898 x 3
#Set parameters
NRuns<-10
LearningRate <- 0.01
#Initialize variables
SumWeights <- rep(0,3)
w<-rep(0,3)

for(j in 1:NRuns){
  for(i in 1:length(y)){
    w <- w - LearningRate*( w%*%(X[i,])-y[i])*X[i,] 
  }
  SumWeights<- (SumWeights + w)
}
wBatch<-SumWeights/NRuns
Loss<-CostFunction(wBatch,X,y)


R_solution<-lm(alcohol~ residualSugar+pH, data=data)