##### Loading libs #####
# You should install those first with install.packages("package")
# general purpose libs
library("plyr")
# parallelisation libs
library("foreach")
library("doParallel")
# predictive algo lib
library("glmnet")

# Running file defining feature extraction functions
source("feature_extraction.R")

##### General settings #####
set.seed(1)
if(!exists("cl")) { # to avoid mistakenly re-running the code
  # CHANGE HERE TO ADAPT THE NUMBER OF WORKERS AND THE NUMBER OF CORES
  cl <- makeCluster(16)
  registerDoParallel(cl, cores=4)
}
##### Settings to tune #####
# number of negative samples
nb.neg.samples <- 200
# GLMNET: elasticnet mixing param (0 is Ridge, 1 is Lasso)
alpha <- 0

# global vars
drivers = list.files("drivers")
labels <- as.factor(c(rep(1,200), rep(0,200)))

# Loading data and converting it to feature space directly
# takes some time
feature_data <- llply(drivers, function(driver) laply(1:200, .fun=function(i)
  extractFeatures(
    array(read.csv(file.path("drivers", as.character(driver), paste0(i, ".csv")))))))

# building the training data for each driver. It is composed of:
# - the trips from the driver (200 positive samples)
# - random trips from random other drivers (nb.neg.samples negative samples)

train.data <- llply(1:length(feature_data), .fun=function(i) {
  # creating negative sample by sampling from other drivers
  randomDrivers <- sample((1:length(drivers))[-i], size = nb.neg.samples, replace=TRUE)
  # samples are directly in feature space
  neg.samples <- laply(randomDrivers, .fun=function(x) feature_data[[x]][sample(200,1),])
  samples <- rbind(feature_data[[i]],neg.samples)
  save(samples, file.path("feature_data", as.character(i)))
  })

# for timing
t <- proc.time()
# Each driver analysis is done in parallel by a different thread.
# Each thread is in its own environment, so we have to redefine everything.
pred <- foreach(i=1:length(drivers), .combine=rbind) %dopar% {
  # "Require" loads the library if it is not already done.
  require("glmnet")
  # this loads "samples" in memory
  load(file.path("feature_data", as.character(i)))
  # fitting the model
  fit <- cv.glmnet(x=samples, y=labels, alpha=alpha, standardize=FALSE, family="binomial")
  # making the prediction. type="response" gives probabilities
  pred <- predict(fit, samples[1:200,], type="response")
}
# display run time
t <- proc.time()-t; t

# generate submission
colnames(pred) <- "prob"
# generates the index as specified by kaggle
submission_index <- unlist(llply(drivers, function(d) paste(d, 1:200, sep="_")))
submission= data.frame(driver_trip=submission_index, prob=1-pred, stringsAsFactors = F)
write.csv(submission, "submissions/featv1_cvglmnet2.csv", row.names=F, quote=F)

# This should be run when you're done
# stopCluster(cl)
