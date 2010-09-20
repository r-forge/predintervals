`plot.predint` <-
function(x, ...){
  obs <- c(x$xhist, x$xtest)
  number <- 1:length(obs)
  numberhist <- 1:x$hist$n
  if (!is.null(x$xtest)) numbertest <- (x$hist$n+1):(x$hist$n+x$test$n)
  PI <- x$PI[x$PI > -Inf & x$PI < Inf]
  minylim <- min(c(obs,unlist(PI)))
  maxylim <- max(c(obs,unlist(PI)))
  plot(obs ~ number, type="n", xaxt="n", ylim=c(minylim, maxylim),
        ylab="Response", xlab="Observation Number")
  if (!is.null(x$xtest)){
    abline(v=x$hist$n+0.5)
    mtext("Historical", side=3, at=1, adj=0, padj=-0.2, cex=0.8)
    mtext("Test", side=3, at=x$hist$n+1, adj=0, padj=-0.2, cex=0.8)
    hx <- axTicks(1)
    hx[1] <- 1
    hx[length(hx)] <- max(number)
    if (x$hist$n-max(hx[hx < x$hist$n]) < 5) hx[sum(hx < x$hist$n)] <- NA
    hx <- na.omit(hx)
    atx <- c(hx[hx < x$hist$n], x$hist$n+1, hx[hx > x$hist$n])
    labx <- c(hx[hx < x$hist$n], 1, hx[hx > x$hist$n]-x$hist$n)
    labx[length(labx)] <- x$test$n
    axis(1, at=atx, labels=labx)
    lines(x$xtest ~ numbertest, pch=19, type="b", col="grey")
  } else axis(1)
  lines(x$xhist ~ numberhist, pch=19, type="b", col="grey")
  points(obs ~ number, pch=19)
  invisible(lapply(PI, function(x) abline(h=x, lwd=2, lty=2)))
  if (x$kwin > 0){
    wpointsy <- c(x$xhist[order(x$xhist)][1:(floor((x$kwin/x$hist$n)*x$hist$n))],x$xhist[order(x$xhist)][(x$hist$n-(floor((x$kwin/x$hist$n)*x$hist$n)-1)):x$hist$n])
    wpointsx <- c(numberhist[order(x$xhist)][1:(floor((x$kwin/x$hist$n)*x$hist$n))],numberhist[order(x$xhist)][(x$hist$n-(floor((x$kwin/x$hist$n)*x$hist$n)-1)):x$hist$n])
    points(wpointsy ~ wpointsx, pch=19, col="lightgrey")#col="red2", pch=21, cex=2, lwd=2)
  }
  if (x$type == "Precedence" & x$distr == "free") segments(min(numbertest),x$test$median,max(numbertest),x$test$median,lwd=2,col="blue")
  if (x$type == "Precedence" & x$distr == "normal") segments(min(numbertest),x$test$mean,max(numbertest),x$test$mean,lwd=2,col="blue")
}
