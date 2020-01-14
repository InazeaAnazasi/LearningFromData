# ------------------------ Learning from Data: Assignment 3 -------------------#
# in this code, I only want to explore the differences between the for-loop
# and a vectorised version of calculating point-symmetry
# 
# ------------------------ Lukas Schmid
#
#_______________________________________________________________________________


# Preliminaries ----------------------------------------------------------------
## loading packages
library(tictoc)
library(microbenchmark)
library(beepr)
# library(dplyr)
# library(tidyr)
# library(purrr)
library(ggplot2)
# library(cowplot) # for more tweaking with graphics (namespace will be given)
# library(magrittr)

## setting the ggplot-theme
# theme_set(theme_minimal())
#_______________________________________________________________________________

#------------------------------------------------------------------------------#
# load the data ----------------------------------------------------------------
#------------------------------------------------------------------------------#

#----------------------------------------------################################-
# prepare the functions that can load the data #################################
#----------------------------------------------################################-
## what follows is copied from https://gist.github.com/brendano/39760

## Load the MNIST digit recognition dataset into R
## http://yann.lecun.com/exdb/mnist/
## assume you have all 4 files and gunzip'd them
## creates train$n, train$x, train$y  and test$n, test$x, test$y
## e.g. train$x is a 60000 x 784 matrix, each row is one digit (28x28)
## call:  show_digit(train$x[5,])   to see a digit.
## brendan o'connor - gist.github.com/39760 - anyall.org

load_mnist <- function() {
  load_image_file <- function(filename) {
    ret = list()
    f = file(filename,'rb')
    readBin(f,'integer',n=1,size=4,endian='big')
    ret$n = readBin(f,'integer',n=1,size=4,endian='big')
    nrow = readBin(f,'integer',n=1,size=4,endian='big')
    ncol = readBin(f,'integer',n=1,size=4,endian='big')
    x = readBin(f,'integer',n=ret$n*nrow*ncol,size=1,signed=F)
    ret$x = matrix(x, ncol=nrow*ncol, byrow=T)
    close(f)
    ret
  }
  load_label_file <- function(filename) {
    f = file(filename,'rb')
    readBin(f,'integer',n=1,size=4,endian='big')
    n = readBin(f,'integer',n=1,size=4,endian='big')
    y = readBin(f,'integer',n=n,size=1,signed=F)
    close(f)
    y
  }
  train <<- load_image_file('mnist/train-images.idx3-ubyte')
  test <<- load_image_file('mnist/t10k-images.idx3-ubyte')
  
  train$y <<- load_label_file('mnist/train-labels.idx1-ubyte')
  test$y <<- load_label_file('mnist/t10k-labels.idx1-ubyte')  
}

show_digit <- function(arr784, col=gray(12:1/12), ...) {
  image(matrix(arr784, nrow=28)[,28:1], col=col, ...)
}

# --------------------------------------------#################################-
# write another function to display the digit ##################################
# --------------------------------------------#################################-
plot_digit <- function(arr784) {
  ggplot(
    data=tibble(x = rep(1:28, times=28), # because arr784 would be filled into
                # a matrix row-wise
                y = rep(28:1, each=28),
                z = arr784)
  ) +
    geom_tile(aes(x, y, fill=z)) +
    scale_fill_gradient(low="#FFFFFF", high="#000000", guide=F) +
    theme_minimal() +
    scale_x_continuous(breaks=NULL, name=NULL) +
    scale_y_continuous(breaks=NULL, name=NULL) +
    theme(panel.border=element_rect(colour="#000000",
                                    fill = NA)) +
    labs(caption=paste("symmetry:", round(calculate_symmetry(arr784), 2),
                       "\nintensity:", calculate_intensity(arr784)))
}

# -----------------------######################################################-
# actually load the data #######################################################
# -----------------------######################################################-
# actually loading the data - we assume that test and train never get separated,
# so they either both exist or don't; when we dont have the data as .RDS-object,
# we generate the data and save it as .RDS; if we have the data, but don't have
# the objects in our workspace, we load the obejct from the data; else, we have
# both and don't have to do anything
if(!file.exists("MINST_rds/03_test_raw.rds") |
   !file.exists("MINST_rds/03_test_raw.rds")) {
  load_mnist()
  saveRDS(train, file = "MINST_rds/train_raw.rds")
  saveRDS(test, file = "MINST_rds/test_raw.rds")
  print("Extracted the data from the original files, saved it as .rds and
        loaded them into the workspace")
} else if (!exists("train") | !exists("test")) {
  test <- readRDS("MINST_rds/03_test_raw.rds")
  train <- readRDS("MINST_rds/03_test_raw.rds")
  print("Files already existed as .rds-files. Only loaded them into the
        workspace.")
} else {
  print("Files and objects in the workspace already existed. Did nothing.")
}


# -----------------------------------------------------------------------------#
# write the function to calculate point symmetry that is not vectorised -------#
# -----------------------------------------------------------------------------#
# this function takes a vector that represents a square matrix and returns the
# the sum of the absolute values of the differences between the every point and
# its mirror point within the square matrix represented by the vector
calculate_pointSymmetryFun <- function(arr784) {
  i_mirror_from_i <- function(i, length=784) { 
    # this function takes an integer that represents the index of an element of
    # a vector that represents a square matrix
    # it returns the integer that represents the index of an element that can be
    # said to be symmetrical to the element represented by the input index on a
    # vertical axis through the middle of the matrix that is represented by the 
    # vector
    # length is the length of the vector that contains the element represented
    # by the index that is the input integer
    N <- sqrt(length)
    m <- (i-1) %% N + 1
    n <- (i-1) %/% N +1
    m_mirror <- N+1 - m
    n_mirror <- N+1 - n
    i <- N*n_mirror + m_mirror - N
    return(i)
  }
  matrixMirrorIndex <- function(nMatrix, matrixSideLength) {
    # we can take every row of the matrix and row-wise fill into a square matrix
    # with side length matrixSideLength
    # get the actual indices 
    indexMatrix <- matrix(1:(matrixSideLength^2),
                          nrow = nMatrix,
                          ncol = matrixSideLength^2,
                          byrow = T)
    # get the row index of an element at position i
    rowMatrix <- matrix()
  }
  # here, we calculate the difference in the hue value between a point a and its
  # symmetric point a' for the first half of the points in a image, since the
  # symmetry test means that all resulting information will be contained within
  # the first half of the vector
  return(sum(abs(arr784[1:784] - arr784[i_mirror_from_i(1:784)])))
}

# -----------------------------------------------------------------------------#
# write the function to calculate point symmetry that is vectorised -----------#
# -----------------------------------------------------------------------------#

calculate_pointSymmetryVect <- function(arr784) {
  return(sum(abs(arr784 - arr784[784:1])))
}


# -----------------------------------------------------------------------------#
# comparing methods using tictoc()
# -----------------------------------------------------------------------------#
  
# using apply and the non-vectorised function
tic(msg="apply")
resApply <- apply(train$x, 1, calculate_pointSymmetryFun)
toc(log=T, quiet=T)

# using sweep and the vectorised function
tic(msg="vector")
resVector <- apply(train$x, MARGIN=1, calculate_pointSymmetryVect)
toc(log=T, quiet=T)

# directly using the matrix, transposing it twice
tic(msg="index")
resIndex <- rowSums(abs(train$x - train$x[, 784:1]))
toc(log=T, quiet=T)

tic("indexHalf")
resIndexHalf <- rowSums(abs(train$x[, 1:392] - train$x[, 784:393]))
toc(log=T, quiet=T)

# compare whether all of them are equal
cat(paste0("apply:     ", length(resApply), "\n",
           "resVector: ", length(resVector), "\n",
           "resIndex:  ", length(resIndex), "\n",
           "resIndexHalf:", length(resIndexHalf)))
all(resApply == resVector)
all(resApply == resIndex)
all(resApply == resIndexHalf * 2)

# get the runtimes
tic.log()
