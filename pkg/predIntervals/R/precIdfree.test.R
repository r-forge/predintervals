`precIdfree.test` <-
function(xhist, xtest, conf.level=0.95, alternative=c("two.sided")){
  if (alternative == "upper" | alternative == "lower") conf.level <- 1-2*(1-conf.level)
  m <- length(xhist)
  n <- length(xtest)
  j <- median(1:n)
  p0 <- conf.level
  w <- 1:m
  ap <- (choose(j+w-1,w) * choose(m+n-j-w,m-w)) / choose(m+n,m)
  a <- 0
  b <- m-a
  while (sum(ap[a:b]) > p0){
    a <- a+1
    b <- m-a
  }
  a <- a-1
  b <- m-a+1
  lower <- xhist[order(xhist)][a]
  upper <- xhist[order(xhist)][b]
  if (alternative == "lower") upper <- Inf
  if (alternative == "upper") lower <- -Inf
  list(lower=lower,upper=upper)
}

