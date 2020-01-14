# ------------------------ Learning from Data: Assignment 3 -------------------#
#
# Lukas Schmid ----
#
# -----------------------------------------------------------------------------#


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
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# Task 2 -----------------------------------------------------------------------
#------------------------------------------------------------------------------#
# In this task you will explore the famous MNIST handwritten digits dataset.
# To load the dataset and display specific handwritten digits use the code
# provided under https://gist.github.com/brendano/39760 .

#------------------------------------------------------------------------------#
# load the data ----------------------------------------------------------------
#------------------------------------------------------------------------------#

#----------------------------------------------################################-
# prepare the functions that can load the data ################################-
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

# -----------------------######################################################-
# actually load the data ######################################################-
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

#------------------------------------------------------------------------------#
# create symmetry --------------------------------------------------------------
#------------------------------------------------------------------------------#
symmetryValues <- rowSums(abs(train$x[, 1:392] - train$x[, 784:393]))
# Symmetric values are at the opposite end of the vector in which they are
# stored. Thus, I take the second half of the vector in descending order and
# subtract it from the first half. Symmetry is the sum of the absolute distances
# of all values.

#------------------------------------------------------------------------------#
# create intensity -------------------------------------------------------------
#------------------------------------------------------------------------------#
intensityValues <- rowSums(train$x)
# Intensity is simply the sum of the intensity values in each digit.

#------------------------------------------------------------------------------#
# compare symmetry and intensity -----------------------------------------------
#------------------------------------------------------------------------------#
# uniform distribution of both variables
data <- tibble(symmetryValues, intensityValues, digit=train$y)
data %>% 
  pivot_longer(cols=c(symmetryValues, intensityValues)) %>% 
  ggplot(aes(x=value, fill=factor(digit), color=factor(digit))) +
  geom_density(alpha=0.3) +
  facet_wrap(facets=vars(name))

# bivariate distribution for only 1s and 5s
data %>% 
  filter(digit==1 | digit==5) %>% 
  ggplot(aes(x=symmetryValues, y=intensityValues,
             fill=factor(digit), color=factor(digit))) +
  geom_point(binwidth=0.1, shape=19, stroke=0, stat="bin2d") +
  geom_density_2d() + 
  scale_fill_manual(values=c("1"="red", "5"="blue")) +
  scale_colour_manual(values=c("1"="red3", "5"="blue3")) +
  scale_alpha_continuous(guide=F) +
  labs(x="Point Symmetry", y="Intensity", colour="Digit", fill="Digit",
       title="Intensity vs. Point Symmetry for digits from the MNIST-dataset",
       caption=paste0("Only 1's and 5's from the test-dataset were used. N = ",
                      nrow(filter(data, digit==1 | digit==5)), ". \n",
                      "Both variables are normalised over all digits 
                      (sd=1, mean=1)")) +
  guides(fill = guide_legend(override.aes = list(size=4)))

# classification performance of a logistic regression for the two types of 
# digits 1 and 5 
## first, we have to transform y so it is either 1 or 0
data <- data %>% 
  filter(digit==1 | digit==5) %>% 
  mutate(y = ifelse(digit==1, 0, 1)) 
  
model <- glm(y ~ symmetryValues+intensityValues, data,
             family="binomial")
summary(model)

#------------------------------------------------------------------------------#
# create other features --------------------------------------------------------
#------------------------------------------------------------------------------#

# remember: my data works row-wise (every row in train$x is one digit), but the
# digits themselves are different: if the values are put into a 28-28-matrix,
# row-wise, then I get the correct image

# prominence of vertical lines
matrix(t(train$x), )

rowIntensity = rowSums(cols)
horizontalityValues = rowMeans(matrix(rowIntensity, nrow=10000))

tibble(horizontalityValues, digit=train$y) %>% 
  ggplot(aes(x=horizontalityValues, fill=factor(digit), color=factor(digit))) +
  geom_density(alpha=0.3)

# prominence of vertical lines

# similarity to typical digits
## create the typical digits: average grey-scale-value for every pixel of 
## a certain digit
average_digits = matrix(0, 10, 784)
  # creating the empty matrix that contains
  # the grey scale values for all the digits
for (digit in 1:10) {
  average_digits[digit,] <- colMeans(train$x[train$y==digit, ])
}

# plotting the average digits
## we need to create a matrix that is made up of smaller matrices
image_matrix <- matrix(0, 28*5, 28*2)
  # the image matrix has 
image(matrix(average_digits[1,], nrow=28)[,28:1], col=gray(12:1/12))

#------------------------------------------------------------------------------#
# other visual inspections -----------------------------------------------------
#------------------------------------------------------------------------------#

# distribution of grey-scale values
plot(as.vector(train$x), type="h") 
