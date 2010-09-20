`predint.default` <-
function(xhist, xtest=NULL, type="Prediction", distr="free", k=length(xtest), conf.level=0.95, alternative="two.sided", direction="increase", MR=FALSE, kwin=0){
  if (kwin > length(xhist)/2-1) kwin <- floor(length(xhist)/2-1)
  if (distr == "normal") ests.hist <- estsInt(xhist, MR=MR, kwin=kwin) else ests.hist <- estsInt(xhist, MR=MR, kwin=0)
  critval <- NULL
  if (!is.null(xtest)) ests.test <- estsInt(xtest, MR=MR, kwin=0) else ests.test <- NULL
  if (is.null(xtest)){
    PI <- predI.hist(ests.hist, conf.level=conf.level, alternative=alternative)
    type <- "Historical only"
    distr <- "Normal"
  }
  if (type == "Prediction"){
    if (distr == "normal") {
      PIr <- predINorm.test(ests.hist, ests.test, k=k, conf.level=conf.level, alternative=alternative)
      critval <- PIr[[3]]
      PI <- PIr[c(1,2)]
    }
    if (distr == "free") PI <- predIdfree.test(xhist, xtest, k=k, conf.level=conf.level, alternative=alternative, direction=direction)
  }
  if (type == "Precedence"){
    if (distr == "normal") PI <- precINorm.test(ests.hist, ests.test, conf.level=conf.level, alternative=alternative)
    if (distr == "free") PI <- precIdfree.test(xhist, xtest, conf.level=conf.level, alternative=alternative)
  }
  names(PI) <- c("lower", "upper")
  if (type == "Prediction"){
    kin <- sum(xtest > PI[1] & xtest < PI[2])
  } else kin <- NULL
  if (type == "Precedence"){
    if (distr == "normal") M <- ests.test$mean else M <- ests.test$median
    if (M > PI[1] & M < PI[2]) Min <- TRUE else Min <- FALSE
  } else Min <- NULL
  pil <- list(xhist=xhist, xtest=xtest, hist=ests.hist, test=ests.test, PI=PI,
      type=type, distr=distr, conf.level=conf.level, k=k, kin=kin, Min=Min, critval=critval, MR=MR, kwin=kwin)
  class(pil) <- "predint"
  return(pil)
}
