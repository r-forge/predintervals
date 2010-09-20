`predI.hist` <-
function(est.hist, conf.level=0.95, alternative=c("two.sided")){
  if (alternative == "two.sided") quant <- qt(1-(1-conf.level)/2, df=est.hist$n-1)
    else quant <- qt(conf.level, df=est.hist$n-1)
  if (alternative == "upper") lower <- -Inf
    else lower <- est.hist$mean - quant * est.hist$sd
  if (alternative == "lower") upper <- Inf
    else upper <- est.hist$mean + quant * est.hist$sd
  list(lower=lower,upper=upper)
}

