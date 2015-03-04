driver = list.files("drivers")[1]
i <- 2

trip <- read.csv(file.path("drivers", as.character(driver), paste0(i, ".csv")))
plot(trip)

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
  geom_point(aes(x=t, y=speed), color="red")+
  geom_point(aes(x=t, y=acc), color="green")

brake <- abs(trip$acc[trip$acc<0])
plot(brake)

plot(abs(fft(trip$speed)))

tmp <- spectrum(trip$speed)
convolve(trip1$speed, trip2$speed)
