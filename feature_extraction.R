FEATURE_EXTRACTION_LOADED <- TRUE

extractFeatures <- function(trip) {
  dx <- c(diff(trip[,1]),0)
  dy <- c(diff(trip[,2]),0)
  d2x <- c(diff(dx),0)
  d2y <- c(diff(dy),0)

  # returns approx. length,
  # speed quantiles
  # positive acceleration quantiles
  # negative acceleration (braking) quantiles
  kin <- getKinetics(dx, dy, d2x, d2y)

  return(c(kin))
}

getKinetics <- function(dx, dy, d2x, d2y)
{
  speed <- sqrt(dx**2+dy**2)
  acc <- c(diff(speed), 0)

  pos.acc.ind <- acc>=0
  pos.acc <- acc[pos.acc.ind]
  neg.acc <- acc[!pos.acc.ind]

  len <- sum(speed)

  dist <- c(
    len,
    quantile(speed, seq(0.05,1, by = 0.05), names=F),
    quantile(pos.acc, seq(0.05,1, by = 0.05), names=F),
    quantile(neg.acc, seq(0.05,1, by = 0.05), names=F)
    )
  names(dist)
}

getCurvature <- function(dx, dy, d2x, d2y) {
  sgn.curv <- (dx*d2y-dy*d2x)/(((dx**2)+(dy**2)+1)**(3/2))
  curv <- abs(sgn.curv)

}
