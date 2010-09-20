`summary.predint` <-
function(object,...){
    cat("\n\t", "Prediction Intervals\n\n")
    cat("Interval Type:", object$type,  "\n")
    cat("Distribution:", object$distr, "\n")
    cat("Confidence Level:", object$conf.level, "\n\n")
    if (object$kwin > 0 & object$distr == "normal" | object$distr == "Normal") cat(paste(object$kwin,"*2 / ",length(object$xhist)," = ",round(((object$kwin*2)/length(object$xhist))*100,1),"% of the historical observations are winsorized. \n\n", sep=""))
    cat("Parameter:", "\n")
    if (object$distr == "free"){
      m <- round(c(object$hist$median,object$test$median),2)
      mt <- "Median"
    } else {
      m <- round(c(object$hist$mean,object$test$mean),2)
      mt <- "Mean"
    }
    if (object$distr == "free"){
      SD <- round(c(object$hist$iqr,object$test$iqr),2)
      SDt <- "IQR"
    } else {
      SD <- round(c(object$hist$sd,object$test$sd),2)
      if (object$MR) SDt <- "MR" else SDt <- "sd"
    }
    n <- c(object$hist$n,object$test$n)
    edat <- data.frame(m,SD,n)
    if (is.null(object$xtest)) rownames(edat) <- c("Historical Data")
      else rownames(edat) <- c("Historical Data", "Test Sample")
    colnames(edat) <- c(mt,SDt,"n")
    print(edat)
    if (object$type == "Prediction") predteobjectt <- c("(to contain at least", object$k, "of", object$test$n, "observations)")
      else predteobjectt <- ""
    cat("\n")
    cat("Interval Limits:", predteobjectt ,"\n")
    print(unlist(object$PI), digits=3)
    cat("\n")
    if (object$type == "Prediction" & object$distr == "normal") cat("Critical value:",object$critval,"\n")
    if (object$type == "Prediction"){
      cat(object$kin, "of", object$test$n, "test observations included in the interval", "\n")
    }
    if (object$type == "Precedence"){
      if (object$distr == "normal") predteobjectt <- paste("Mean of the test sample (",round(object$test$mean,2),")", sep="")
        else predteobjectt <- paste("Median of the test sample (",round(object$test$median,2),")", sep="")
      if (object$Min) cat(predteobjectt, "included in the interval", "\n")
        else  cat(predteobjectt, "NOT included in the interval", "\n")
    }
    invisible(object)
}


