##### Loading libs #####
# You should install those first with install.packages("package")
# general purpose libs
library("plyr")
# parallelisation libs
library("foreach")
library("doParallel")
# predictive algo lib
library("e1071")

# Running file defining feature extraction functions
source("feature_extraction.R")

##### General settings #####
set.seed(1)
if(!exists("cl")) { # to avoid mistakenly re-running the code
  # CHANGE HERE TO ADAPT THE NUMBER OF WORKERS AND THE NUMBER OF CORES
  cl <- makeCluster(6)
  registerDoParallel(cl, cores=4)
}
##### Settings to tune #####
# number of negative samples
nb.neg.samples <- 400

# global vars
drivers = list.files("drivers")[1:10]
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
# should take about 10 minutes...
feature_data <- llply(drivers, .progress="text", .parallel=FALSE,
                      .paropts=list(.export=, .packages="quantmod"),
                      .fun=function(driver) {laply(1:200, .fun=function(i) readFeatures(driver, i))})
t_feat <- proc.time()-t_feat; t_feat

### saving the features to disk to avoid recomputing
# feature_data_table <- foreach(x=feature_data, .combine=rbind) %do% x
# submission_index <- unlist(llply(drivers, function(d) paste(d, 1:200, sep="_")))
# feature_data_table <- data.frame(driver_trip=submission_index, feature=feature_data_table)
# write.csv(feature_data_table, file="features.csv")

# building the training data for each driver. It is composed of:
# - the trips from the driver (200 positive samples)
# - random trips from random other drivers (nb.neg.samples negative samples)
train.data <- llply(1:length(feature_data), .fun=function(i) {
  # creating negative sample by sampling from other drivers
  randomDrivers <- sample((1:length(drivers))[-i], size = nb.neg.samples, replace=TRUE)
  # samples are directly in feature space
  neg.samples <- laply(randomDrivers, .fun=function(x) feature_data[[x]][sample(200,1),])
  samples <- rbind(feature_data[[i]],neg.samples)
  })

# for timing
t_pred <- proc.time()
# driver analysis is done in parallel.
pred <- foreach(data=train.data, .combine=c,
                .packages="e1071") %dopar% {
  # fitting the model (using cross-validation to tune lambda parameter)
  fit <- svm(x=data, y=labels, type="C-classification", probability=TRUE)
  # making the prediction. type="response" gives probabilities
  pred <- predict(fit, data[1:200,], probability=TRUE)
  prob <- attr(pred, "probabilities")[,1]
}
# display run time
t_pred <- proc.time()-t_pred; t_pred

# generates the index as specified by kaggle
submission_index <- unlist(llply(drivers, function(d) paste(d, 1:200, sep="_")))
submission= data.frame(driver_trip=submission_index, prob=pred, stringsAsFactors = F)
write.csv(submission, "submissions/featv2_cvglmnet4_alpha05.csv", row.names=F, quote=F)

# This should be run when you're done
# stopCluster(cl)
