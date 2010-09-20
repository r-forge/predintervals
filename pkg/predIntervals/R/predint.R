`predint` <-
function(xhist, xtest=NULL, type="Prediction", distr="free", k=length(xtest), conf.level=0.95, alternative="two.sided", direction="increase", MR=FALSE, kwin=0) UseMethod("predint")
