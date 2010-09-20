`predINorm.test` <-
function(est.hist, est.test, k, conf.level=0.95, alternative=c("two.sided")){
  PIcritval <- function(k,m,n,alpha){
    nu <- n - 1
    p <- 1/(n + 1)
    fnu <- function(s) {
      2 * nu * s * dchisq(nu * s^2, n-1)
    }
    g <- function(s, j, u) {
      integrate(function(t) {
        sapply(t, function(t) {
          a <- (-u * s + (p)^(0.5) * t)/((1 - p)^(0.5))
          b <- (u * s + (p)^(0.5) * t)/((1 - p)^(0.5))
          w <- pnorm(b) - pnorm(a)
          choose(m, j) * (w)^j * (1 - w)^(m - j) * dnorm(t)
       })
      }, -Inf, Inf)$value * fnu(s)
    }
    PB <- function(j, u) {
      integrate(function(s) {
        sapply(s, function(s) {
          g(s, j, u)
        })
      }, 0, Inf)$value
    }
    h <- function(u) {
      sapply(u, function(u) {
        (sum(sapply(k:m, function(j) PB(j, u)))) - alpha
      })
    }
    ustar <- uniroot(h, c(1, 10))$root
    sqrt((n+1)/n)*ustar
  }
  PIonesided <- function(k,m,n,alpha){
    nu <- n - 1
    p <- 1/(n + 1)
    fnu <- function(s) {
      2 * nu * s * dchisq(nu * s^2, n-1)
    }
    g <- function(s, j, u) {
      integrate(function(t) {
        sapply(t, function(t) {
          b <- (u * s + (p)^(0.5) * t)/((1 - p)^(0.5))
          w <- pnorm(b)
          choose(m, j) * (w)^j * (1 - w)^(m - j) * dnorm(t)
        })
      }, -Inf, Inf)$value * fnu(s)
    }
    PB <- function(j, u) {
      integrate(function(s) {
        sapply(s, function(s) {
          g(s, j, u)
        })
      }, 0, Inf)$value
    }
    h <- function(u) {
      sapply(u, function(u) {
        (sum(sapply(k:m, function(j) PB(j, u)))) - alpha
      })
    }
    ustar <- uniroot(h, c(1, 10))$root
    sqrt((n+1)/n)*ustar
  }
  if (alternative == "two.sided") quant <- PIcritval(k=k,m=est.test$n,n=est.hist$n,alpha=conf.level)
    else quant <- PIonesided(k=k,m=est.test$n,n=est.hist$n,alpha=conf.level)
  if (alternative == "upper") lower <- -Inf
    else lower <- est.hist$mean - quant * est.hist$sd * sqrt(1 + (1/est.hist$n))
  if (alternative == "lower") upper <- Inf
    else upper <- est.hist$mean + quant * est.hist$sd * sqrt(1 + (1/est.hist$n))
  list(lower=lower,upper=upper, critval=quant)
}

