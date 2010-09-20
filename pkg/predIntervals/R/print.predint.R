`print.predint` <-
function(x, ...){
    cat("\n\t", "Prediction Intervals\n\n")
    cat("Interval Type:", x$type,  "\n")
    cat("Distribution:", x$distr, "\n")
    cat("Confidence Level:", x$conf.level, "\n\n")
    if (x$kwin > 0 & x$distr == "normal" | x$distr == "Normal") cat(paste(x$kwin,"*2 / ",length(x$xhist)," = ",round(((x$kwin*2)/length(x$xhist))*100,1),"% of the historical observations are winsorized.\n\n", sep=""))
    if (x$type == "Prediction") predtext <- c("(to contain at least", x$k, "of", x$test$n, "observations)")
      else predtext <- ""
    cat("Interval Limits:", predtext ,"\n")
    print(unlist(x$PI), digits=3)
    cat("\n")
    if (x$type == "Prediction"){
      cat(x$kin, "of", x$test$n, "test observations included in the interval", "\n")
    }
    if (x$type == "Precedence"){
      if (x$distr == "normal") predtext <- paste("Mean of the test sample (",round(x$test$mean,2),")", sep="")
        else predtext <- paste("Median of the test sample (",round(x$test$median,2),")", sep="")
      if (x$Min) cat(predtext, "included in the interval", "\n")
        else  cat(predtext, "NOT included in the interval", "\n")
    }
    invisible(x)
}


