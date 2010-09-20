`precINorm.test` <-
function(est.hist, est.test, conf.level=0.95, alternative=c("two.sided")){
  if (alternative == "two.sided") quant <- qt(1-(1-conf.level)/2, df=est.hist$n-1)
    else quant <- qt(conf.level, df=est.hist$n-1)
  if (alternative == "upper") lower <- -Inf
    else lower <- est.hist$mean - quant * est.hist$sd * sqrt((1/est.test$n) + (1/est.hist$n))
  if (alternative == "lower") upper <- Inf
    else upper <- est.hist$mean + quant * est.hist$sd * sqrt((1/est.test$n) + (1/est.hist$n))
  list(lower=lower,upper=upper)
}

