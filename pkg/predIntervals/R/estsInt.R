`estsInt` <-
function(x, MR=FALSE, kwin){
  mx <- mean(x)
  nx <- length(x)
  medx <- median(x)
  IQRx <- IQR(x)
  if (MR == FALSE) sdx <- sd(x)
    else sdx <- 0.886*(1/(nx-1))*sum(abs(x[2:nx] - x[1:(nx-1)]))
  winz <- function(x,wa){
    m <- length(x)
    am <- floor(wa*m)
    wmx <- 1/m * ((1+am) * (x[(1+am)] + x[m-am]) + sum(x[(am+2):(m-am-1)]))
    SQw <- (1+am) * ((x[am+1]-wmx)^2 + (x[m-am]-wmx)^2) + sum((x[(am+2):(m-am-1)]-wmx)^2)
    MQw <- SQw/m
    c(wmx, MQw)
  }
  if (kwin > 0){
    winsor <- winz(x[order(x)],kwin/nx)
    mx <- winsor[1]
    sdx <- sqrt(winsor[2])
  }
  list(mean=mx,sd=sdx,n=nx,median=medx,iqr=IQRx)
}
