driver <- 1
i <- 90
trip <- array(read.csv(file.path("drivers", as.character(driver), paste0(i, ".csv"))))
plot(trip)

extractFeatures(trip)
# computing velocity and acceleration (padding wth 0 at the end)
trip$dx <- c(diff(trip$x),0)
trip$dy <- c(diff(trip$y),0)
trip$d2x <- c(diff(trip$dx),0)
trip$d2y <- c(diff(trip$dy),0)

trip$speed <- sqrt(trip$dx**2+trip$dy**2)
trip$acc <- c(diff(trip$speed), 0)
trip$acc2 <- sqrt(trip$d2x**2+trip$d2y**2)
trip$jerk <- c(diff(trip$acc),0)
# computing curvature
trip$sgn.curv <- (trip$dx*trip$d2y-trip$dy*trip$d2x)/(((trip$dx**2)+(trip$dy**2)+1)**(3/2))
trip$curv <- abs(trip$sgn.curv)

library("ggplot2")
t <- 1:dim(trip)[1]
ggplot(trip)+
  geom_point(aes(x=x, y=y), color="blue")

ggplot(trip)+
  geom_point(aes(x=t, y=curv), color="blue")+
  geom_point(aes(x=t, y=speed), color="red")
  geom_point(aes(x=t, y=acc), color="green")

require("foreach")

brake <- abs(trip$acc[trip$acc<0])
#faster than quantile to get the 90% quantile
brake.breakpoint <- quantile(brake, seq(0,1,0.1))[10]
brake.hard.ind <- which(brake>brake.breakpoint)

library("NbClust")
NbClust(cbind(brake), brake.episode), method="kmeans")

hist(trip$curv)
ecdf(trip$curv)(0)

s <- quantile(speed, probs=seq(0,1,0.1))
hist(speed, breaks=20)
plot(speed)
plot(s)


plot(abs(fft(trip$speed)))
tmp <- spectrum(trip$speed)
convolve(trip1$speed, trip2$speed)

library("quantmod")
library(ggplot2)
plotPeak <- function(y, ...) {
  peak <- rep(0,length(y))
  peak[findPeaks(y, ...)] <- 1
  print(sum(peak))
  data <- data.frame(x=1:length(y), y=y,peak=peak)
  ggplot(data)+geom_point(aes(x=x,y=y,color=peak))
}

plotPeak(curv)
