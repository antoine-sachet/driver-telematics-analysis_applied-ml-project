# Loading libs
library("plyr")
library("glmnet")
library("foreach")
library("doParallel")
source("helperfun.R")
# General settings
set.seed(1)
cl <- makeCluster(32)
registerDoParallel(cl, cores=4)

##### Settings to tune
# number of negative samples
nb.neg.samples <- 200
# elasticnet mixing param (0 is Ridge, 1 is Lasso)
alpha <- 0

# global vars
drivers = list.files("drivers")
labels <- as.factor(c(rep(1,200), rep(2,200)))

# Loading data and converting it to feature space directly
feature_data <- llply(drivers, function(driver) laply(1:200, .fun=function(i)
  extractFeatures(
    array(read.csv(file.path("drivers", as.character(driver), paste0(i, ".csv")))))))

# building negative samples for each driver and writing them to disk
dir.create("feature_data")
l_ply(1:length(feature_data), .fun=function(i) {
  # creating negative sample by sampling from other drivers
  randomDrivers <- sample((1:length(drivers))[-i], size = nb.neg.samples, replace=TRUE)
  # samples are directly in feature space
  neg.samples <- laply(randomDrivers, .fun=function(x) feature_data[[x]][sample(200,1),])
  samples <- rbind(feature_data[[i]],neg.samples)
  save(samples, file=file.path("feature_data", as.character(i)))
  })

t <- proc.time()
pred <- foreach(i=1:length(drivers), .combine=rbind) %dopar% {
  require("glmnet")
  # this loads "samples" in memory
  load(file.path("feature_data", as.character(i)))
  # positive samples are from the current (i-th) driver
  # features are directly extracted
  fit <- cv.glmnet(x=samples, y=labels, alpha=0.5, standardize=FALSE, family="binomial")
  print(i)
  pred <- predict(fit, samples[1:200,], type="response")
}
proc.time()-t

colnames(pred) <- "prob"
submission_index <- unlist(llply(drivers, function(d) paste(d, 1:200, sep="_")))
submission= data.frame(driver_trip=submission_index, prob=1-pred, stringsAsFactors = F)
write.csv(submission, "submissions/featv1_cvglmnet2.csv", row.names=F, quote=F)
