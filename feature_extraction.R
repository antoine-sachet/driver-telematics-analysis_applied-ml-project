extractFeatures <- function(trip) {
  speedDistribution(trip)
}

speedDistribution <- function(trip)
{
  speed = 3.6*sqrt(diff(trip[,1],20,1)^2 + diff(trip[,2],20,1)^2)/20
  return(quantile(speed, seq(0.05,1, by = 0.05)))
}
