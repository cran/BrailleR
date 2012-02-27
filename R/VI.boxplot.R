VI.boxplot <-
function (x, ...) 
{
    args = list(...)
    BPlot = boxplot(x, args)
    if (dim(BPlot$stats)[2] > 1) {
        if (length(BPlot$out) > 0) {
            PossOuts = rep(0, length(BPlot$names))
            names(PossOuts) = BPlot$names
            Outs = tapply(BPlot$out, BPlot$group, length)
            PossOuts[as.numeric(names(Outs))] = Outs
            cat("The number of outliers in each group is:")
            cat(paste("\nGroup ", BPlot$names, " has ", PossOuts, 
                " outliers out of ", BPlot$n, " observations.", 
                sep = ""))
            cat("\n")
        }
        else {
            if (var(BPlot$n) > 0) {
                cat("There are no outliers in any group. The group sizes are:")
                cat(paste("\nGroup ", BPlot$names, " has ", BPlot$n, 
                  " observations.", sep = ""))
                cat("\n")
            }
            else {
                cat(paste("All groups are of size", BPlot$n[1], 
                  ".\n", sep = ""))
            }
        }
    }
    else {
        cat(paste("Of the ", BPlot$n, " observations in the data, ", 
            length(BPlot$out), " have been flagged as outliers.\n", 
            sep = ""))
    }
}
