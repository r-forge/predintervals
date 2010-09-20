`predIdfree.test` <-
function(xhist, xtest, k, direction="increase", conf.level=0.95, alternative=c("two.sided")){
  distfreetollim <- function(r, m, n, K){
    N0 <- 0:n
    PN0 <- (choose(N0 + m - r, N0) * choose(n - N0 + r - 1, n - N0)) / (choose(n + m, n))
    PN0i <- PN0[(n+1):1]
    sum(cumsum(PN0i) >= K)-1
  }
  m <- length(xhist)
  n <- length(xtest)
  dfl <- numeric(length=m)
  for (r in 1:m){
    dfl[r] <- distfreetollim(r=r, m=m, n=n, K=conf.level)
  }
  nu <- sum(dfl >= k)
  if (alternative == "two.sided"){
    if (direction == "decrease"){
      v1 <- ceiling(nu/2)
      v2 <- floor(nu/2)
      if (nu == 0){
        v1 <- 1
        v2 <- 1
      }
      lower <- xhist[order(xhist)][v1]
      upper <- xhist[order(xhist)][m-v2+1]
    }
    if (direction == "increase"){
      v1 <- floor(nu/2)
      v2 <- ceiling(nu/2)
      if (nu == 0){
        v1 <- 1
        v2 <- 1
      }
      if (v1 == 0){
        v1 <- 1
      }
      lower <- xhist[order(xhist)][v1]
      upper <- xhist[order(xhist)][m-v2+1]
    }
  }
  if (alternative == "lower"){
    if (nu == 0) nu <- 1
    lower <- xhist[order(xhist)][nu]
    upper <- Inf
  }
  if (alternative == "upper"){
    if (nu == 0) nu <- 1
    lower <- -Inf
    upper <- xhist[order(xhist)][m-nu+1]
  }
  list(lower=lower,upper=upper)
}

