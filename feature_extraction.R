library("quantmod")

FEATURE_EXTRACTION_LOADED <- TRUE

extractFeatures <- function(trip) {
  if(any(is.na(trip)))
    write.csv(trip, file="shit.csv")

  dx <- c(diff(trip[,1]),0)
  dy <- c(diff(trip[,2]),0)
  d2x <- c(diff(dx),0)
  d2y <- c(diff(dy),0)
  # signed curvature
  sgn.curv <- (dx*d2y-dy*d2x)/(((dx**2)+(dy**2)+0.001)**(3/2))

  # returns: approx. length,
  # speed quantiles
  # positive acceleration deciles (10 quantiles)
  # negative acceleration (braking) deciles
  kin <- getKinetics(dx, dy, d2x, d2y)
  # duration = number of time steps
  duration <- dim(trip)[1]
  # number of left and right turns
  # for the trip divided in 3 equal length sections
  nb.turns <- getTurns(sgn.curv)

  return(c(kin, duration, nb.turns))
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
    unlist(quantile(speed, seq(0.1, 1, by = 0.1), names=F)),
    unlist(quantile(pos.acc, seq(0.1, 1, by = 0.1), names=F)),
    unlist(quantile(neg.acc, seq(0.1, 1, by = 0.1), names=F))
    )
}

getTurns <- function(sgn.curv) {
  pos.curv.ind <- sgn.curv>0
  pos.curv <- sgn.curv[pos.curv.ind]
  neg.curv <- -sgn.curv[!pos.curv.ind]

  br <- round(length(pos.curv)/3)
  neg.br <- round(length(neg.curv)/3)
  split.curv <- list(pos.curv[1:br],
                     pos.curv[(br+1):(2*br)],
                     pos.curv[(2*br+1):length(pos.curv)],
                     neg.curv[1:neg.br],
                     neg.curv[(neg.br+1):(2*neg.br)],
                     neg.curv[(2*neg.br+1):length(neg.curv)])
  laply(split.curv, function(x) {
    res <- 0
    if (any(is.na(x)) | length(x)==0) {
        res <- 0
      } else {
      res <- tryCatch(length(findPeaks(x, thresh=quantile(x,0.8))),
                      error = function(e) {print(e)
                                         write.csv(sgn.curv, file="shit.csv")},
                      warning = function(w) {print(w)
                                           write.csv(sgn.curv, file="shit.csv")}
      )
      }
      return(res)
  })
}

getLongestLine <- function(sgn.curv) {
  line <- sgn.curv<quantile(sgn.curv, 0.2)
}
