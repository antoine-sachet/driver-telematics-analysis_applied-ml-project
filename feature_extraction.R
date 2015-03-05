FEATURE_EXTRACTION_LOADED <- TRUE

extractFeatures <- function(traj) {
  dx <- c(diff(traj[,1]),0)
  dy <- c(diff(trip[,2]]),0)
  d2x <- c(diff(dx),0)
  d2y <- c(diff(dy),0)

  speed <- sqrt(dx**2+trip$dy**2)
  acc <- c(diff(speed), 0)
  jerk <- c(diff(trip$acc),0)
}

kinetics <- function(trip)
{
  speed = 3.6*sqrt(diff(trip[,1],20,1)^2 + diff(trip[,2],20,1)^2)/20
  return(quantile(speed, seq(0.05,1, by = 0.05)))
}
