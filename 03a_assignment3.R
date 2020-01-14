# ------------------------ Learning from Data: Assignment 3 -------------------#
#
# ------------------------ Lukas Schmid
#
#_______________________________________________________________________________


# Preliminaries ----------------------------------------------------------------
## loading packages
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(cowplot) # for more tweaking with graphics (namespace will be given)
library(magrittr)

## setting the ggplot-theme
theme_set(theme_minimal())
#_______________________________________________________________________________

# Task 2 -----------------------------------------------------------------------
# In this task you will explore the famous MNIST handwritten digits dataset.
# To load the dataset and display specific handwritten digits use the code
# provided under https://gist.github.com/brendano/39760 .

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
# generate features ------------------------------------------------------------
# -----------------------------------------------------------------------------#

# ----------------------------#################################################-
# generate the symmetry value ##################################################
# ----------------------------#################################################-
## a image symmetry value of 1 would mean perfect symmetry, meaning the left and
## the right side are identical apart from being mirrored
## broken down to single points: two points have a symmetry value of 1 if they
## are in the same row, have the same distance to the symmetry line, and have
## the same hue value; the pointwise symmetry value is the normalised difference
## between the hue-values of two points; it ranges from 0 to 255
## to calculate the image symmetry value, we do a pairwise comparison of all
## points in the same row and the same distance from the middle with regards
## to their hue-value 
calculate_verticalSymmetry <- function(arr784) {
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
    m_mirror <- m
    n_mirror <- N+1 - n
    i <- N*n_mirror + m_mirror - N
    return(i)
  }
  # here, we calculate the difference in the hue value between a point a and its
  # symmetric point a' for the first half of the points in a image, since the
  # symmetry test means that all resulting information will be contained within
  # the first half of the vector
  return(sum(abs(arr784[1:784] - arr784[i_mirror_from_i(1:784)])))
}

# ------------------------------###############################################-
# generate the intensity values ################################################
# ------------------------------###############################################-
# this function now takes a matrix as an input and directly return the
# rowsums using rowSums
calculate_intensity <- function(matrix784) {
  return(rowSums(matrix784))
}

## ------------------------------##############################################-
## generate point symmetry value ###############################################
## ------------------------------##############################################-
calculate_pointsymmetry <- function(arr784) {
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

# calculate intensity and symmetry values for all digits in the training data
apply(train$x[1:10,], calculate_verticalSymmetry)


## visualise the distribution of both variables:
ggplot(sample_data, aes(x=symmetry, y=intensity, colour=y, fill=y)) +
  geom_point() +
  facet_wrap(vars(y))

## the symmetry values seem odd; 
## let's first check the overall distribution of symmetry
ggplot(data, aes(x=symmetry, color=y, fill=y)) +
  geom_density(alpha=0.3)
## let's compare the digits with the highest 
## symmetry to those with the smalles symmetry
symmetry_comparison <- bind_rows(
  top_n(data, 10, symmetry),
  top_n(data, -10, symmetry))

symmetry_comparison_plotlist <- map(symmetry_comparison$data,
                                    function(x) plot_digit(x))
title <- cowplot::ggdraw() +
  cowplot::draw_label("Digits from the MNIST-training-data with the 10 highest and lowest symmetry values.")
caption <- cowplot::ggdraw() +
  cowplot::draw_label("Symmetry values are reversed, i.e. high values mean little symmetry and vice versa.", size=10)

cowplot::plot_grid(
  title,
  cowplot::plot_grid(plotlist = symmetry_comparison_plotlist,
                           nrow=2),
  caption,
  nrow=3,
  rel_heights = c(0.1, 1, 0.1))

## as we can see, pretty "symmetric" numbers (by intuitive judgement) receive
## generally low symmetry scores (keep in mind that the score is reversed)



## -------------------------------------#######################################-
## appy both values to the training data #######################################
## -------------------------------------#######################################-

data$point_symmetry <- map_dbl(data$data, 
                               function(data) calculate_pointsymmetry(data))

## ----------------------------------------------------------------------------#
## visually inspect distribution of pointsymmetry ------------------------------
## ----------------------------------------------------------------------------#
## getting density plots of point-symmetry values for the different digits
ggplot(data, aes(x=point_symmetry, color=y, fill=y)) +
  geom_density(alpha=0.3)
## now, 1 stands out against all other numbers
## visualise the distribution of point symmetry and intensity in scatterplot:
ggplot(data, aes(x=point_symmetry, y=intensity, colour=y, fill=y)) +
  geom_point() +
  facet_wrap(vars(y))
## still not what I expect: 5 and 1 should be distinctly different! plot them
## over each other:
filter(data, y=="1" | y=="5") %>% 
  ggplot(aes(x=point_symmetry, y=intensity, colour=y, fill=y)) +
  geom_point()

## ----------------------------------------------------------------------------#
## prepare data object for further inspection ----------------------------------
## ----------------------------------------------------------------------------#
z_standard <- function(x) {
  x <- (x - mean(x))/sd(x)
}
clean_data <- data %>% 
  select(intensity, point_symmetry, y) %>% 
  mutate(point_symmetry = z_standard(point_symmetry),
         intensity = z_standard(intensity))

## ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
## graphically show the difference between 1's and 5's 
## ____________________________________________________________________________ 

filter(clean_data, y=="1" | y=="5") %>% 
  ggplot(aes(x=point_symmetry, y=intensity, fill=y, color=y)) +
  geom_point(aes(alpha=..count..), stat="bin2d", binwidth=0.1, shape=19, stroke=0) +
  geom_density_2d() + 
  scale_fill_manual(values=c("1"="red", "5"="blue")) +
  scale_colour_manual(values=c("1"="red3", "5"="blue3")) +
  scale_alpha_continuous(guide=F) +
  labs(x="Point Symmetry", y="Intensity", colour="Digit", fill="Digit",
       title="Intensity vs. Point Symmetry for digits from the MNIST-dataset",
       caption=paste0("Only 1's and 5's from the test-dataset were used. N = ",
                      nrow(filter(clean_data, y=="1" | y=="5")), ". \n",
                      "Both variables are normalised over all digits 
                      (sd=1, mean=1)")) +
  guides(fill = guide_legend(override.aes = list(size=4)))
  