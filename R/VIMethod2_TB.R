
VI.aov <- function(x) # Last Edited: 18/02/15
{
    GroupL <- interaction(x$model[,-1],sep=":")
    FNames = attr(x$terms, 'term.labels')
    for (Factor in FNames)
    {	
	cat("\nThe p value for", Factor , "is", round(anova(x)[which(FNames==Factor), 5], 4)) 
    }
      cat("\n\nThe ratios of the group standard deviations to the overall standard deviation \n", 
        "(groups ordered by increasing mean) are:\n", round(tapply(x$residuals, 
            GroupL, sd)/sqrt(sum(x$residuals^2)/x$df.residual), 2))
    cat("\n  \n")
    return(invisible(NULL))
}




VI.TukeyHSD <- function(x) # Last Edited: 25/02/15
{
  CI <- attr(x, 'conf.level')
  for(Comparison in 1:length(x))
  {
	  CTable = x[[names(x)[Comparison]]]
	  SignPValues = (CTable[,4] <= (1-CI))
	  if (any(SignPValues==TRUE))
	  { 
	    cat("For term ", names(x)[Comparison], " the comparisons which are significant to ", (1-CI)*100, "% are:\n", sep="") 
	    for (DRow in 1:length(SignPValues))
        {
          if(SignPValues[DRow]==TRUE)
	    {
		  SComp = strsplit(rownames(CTable)[DRow], "-")[[1]]
		  cat(SComp[1],"and",SComp[2], "with a difference of",
		  round(CTable[DRow,1],2), "and P-value of", round(CTable[DRow,4],4),"\n")
	    }
 	  }
	}
	else{ cat("For term ", names(x)[Comparison], " there are no comparisons significant to ", (1-CI)*100, "%\n", sep="") }
	cat("\n")
  }
return(invisible(NULL))
}
