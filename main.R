# This code can be found at
# https://github.com/antoine-sachet/driver-telematics-analysis_applied-ml-project
# along with some explanations
#
# Author: Antoine Sachet
# Date: 17/03/2015

##### Settings to tune #####
# number of negative samples (vs always 200 positive samples)
nb.neg.samples <- 200
# weights of the positive (labeled 1) and negative (labeled 0) samples
# default is 1 for both
w <- c(1,1)
names(w) <- c("1", "0")
# This script uses svm with gaussian kernel
# range of gamma (width of gaussian kernel) to try (can be more than 1, best is retained)
gamma <- c(1/38)
# range of cost (C of the C-classification, see help of svm)
# to try (can be more than 1, best is retained)
C <- c(1)
# n-fold cross validation is used to select best gama and C
# number of folds to use
nfold <- 5
# number of workers to create and number of core to use
nworkers <- 3
ncores <- 3

##### Loading libs #####
# You should install those first with install.packages("package")

# "doParallel" is used to register the parallel back-end on windows
# on unix, consider dusing "doMC" instead
library("doParallel")

# "plyr" defines custom "apply" functions
# which can work on any data structure and return any data structure
# Bonus: very easily parallelized once there is a parallel back-end
library("plyr")

# "foreach" allow the use of parallel for-loop
# (which are just convenient wrappers of apply functions)
# requires a parallel back-end for %dopar% to work
library("foreach")

# svm lib
library("e1071")

# only for the "findPeaks" function used in feature_extraction.R
library("quantmod")

# Running file defining feature extraction functions
source("feature_extraction.R")

##### General settings #####
set.seed(1)
if(!exists("cl")) { # to avoid mistakenly re-running the code
  cl <- makeCluster(nworkers)
  registerDoParallel(cl, cores=ncores)
}

# global vars
drivers = list.files("drivers")
labels <- as.factor(c(rep(1,200), rep(0,nb.neg.samples)))

# Read i-th trip of specified driver from disk
# Convert it to feature space directly
readFeatures <- function(driver, i) {
  # this is useful if readFeatures is executed in parallel in different environments
  # The flag FEATURE_EXTRACTION_LOADED is set at the end of feature_extraction.R
  if (!exists("FEATURE_EXTRACTION_LOADED"))
    source("feature_extraction.R")
  print(paste0(driver, "_", i))
  # note that we load the data in an array which is cheaper to process than a table
  extractFeatures(array(read.csv(file.path("drivers", as.character(driver), paste0(i, ".csv")))))
}

# for every driver, for i=1:200, apply readFeature to the i-th trip
# Only the features are in memory afterwards, not the data which would be huge.
# this is parallelised over the drivers only
t_feat <- proc.time()
print("Computing the features...")
# should take less than an hour with the appropriate number of cores
feature_data <- llply(drivers, .parallel=TRUE,
                      .paropts=list(.export="readFeatures", .packages="quantmod"),
                      .fun=function(driver) {
                        unname(laply(1:200, .fun=function(i) readFeatures(driver, i)))})
t_feat <- proc.time()-t_feat; t_feat

### saving the features to disk to avoid recomputing
feature_data_table <- foreach(x=feature_data, .combine=rbind) %do% x
submission_index <- unlist(llply(drivers, function(d) paste(d, 1:200, sep="_")))
feature_data_table <- data.frame(driver_trip=submission_index, feature=feature_data_table)
write.csv(feature_data_table, file="features20.csv")
rm(feature_data_table)

# building the training data for each driver. It is composed of:
# - the trips from the driver (200 positive samples)
# - random trips from random other drivers (nb.neg.samples negative samples)
print("Generating train data")
train.data <- llply(1:length(feature_data), .fun=function(i) {
  # creating negative sample by sampling from other drivers
  randomDrivers <- sample((1:length(drivers))[-i], size = nb.neg.samples, replace=TRUE)
  # samples are directly in feature space
  neg.samples <- laply(randomDrivers, .fun=function(x) feature_data[[x]][sample(200,1),])
  samples <- rbind(feature_data[[i]],neg.samples)
})

# timing the prediction
t_pred <- proc.time()
print("Training the SVMs (using cv to choose best model) and making prediction...")
# driver analysis is done in parallel.
pred <- foreach(data=train.data, .combine=c, .packages="e1071") %dopar% {
  # fitting the model (using cross-validation to tune parameters)
  cv.fit <- tune(svm, train.x=data, train.y=labels,
                 ranges = list(gamma = gamma, cost=C),
                 type="C-classification", probability=TRUE,
                 class.weights=w,
                 tunecontrol = tune.control(sampling="cross", cross=nfold))
  # retaining best model
  fit <- cv.fit$best.model
  # making the prediction. type="response" adds probabilities as attribute
  pred <- predict(fit, data[1:200,], probability=TRUE)
  # extracting said attribute
  prob <- attr(pred, "probabilities")[,1]
}
# display run time
t_pred <- proc.time()-t_pred; t_pred

# generates the index as specified by kaggle
submission_index <- unlist(llply(drivers, function(d) paste(d, 1:200, sep="_")))
submission= data.frame(driver_trip=submission_index, prob=pred, stringsAsFactors = F)
# the folder "submissions" should exist.
# otherwise create it with dir.create("submissions") or just save somewhere else.
write.csv(submission, "submissions/submission1.csv", row.names=F, quote=F)

# This should be run when you're done
stopCluster(cl)
