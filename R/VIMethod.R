

VI = function(x){
UseMethod("VI")
}

VI.default=function(x){
cat("There is no specific method written for  this type of object.\n")
cat("You might try to use the print() function on the object or the str() command to investigate its contents.\n")
print(x)
}


VI.boxplot = function(x){
NBox=length(x$n)
VarGroup = ifelse(NBox>1, "group", "variable")
IsAre = ifelse(NBox>1, "are", "is")
Boxplots = ifelse(NBox>1, paste(NBox, "boxplots"), "a boxplot")
VertHorz = ifelse(x$horizontal, "horizontally", "vertically")

cat("This graph has", Boxplots, "printed", VertHorz, "\n")
cat("with the title:", x$main, "\n",
x$xlab, "is marked on the x-axis.\n",
x$ylab, "is marked on the y-axis.\n")
for(i in 1:NBox){
cat("This", VarGroup, "has", x$n[i], "values.\n") 
if(any(x$group == i)){
cat("An outlier is marked at:", x$out[which(x$group == i)], "\n")}
else{cat("There are no outliers marked for this", VarGroup, "\n")}
cat("The whiskers extend to", x$stats[1,i], "and", x$stats[5,i], "from the ends of the box, \nwhich are at", x$stats[2,i], "and", x$stats[4,i], "\n")
BoxLength=x$stats[4,i]-x$stats[2,i]
cat("The median,", x$stats[3,i], "is", round(100* (x$stats[3,i]-x$stats[2,i])/BoxLength,0), 
"% from the lower end of the box to the upper end.\n")
cat("The upper whisker is", round((x$stats[5,i]-x$stats[4,i])/(x$stats[2,i]-x$stats[1,i]),2), "times the length of the lower whisker.\n")
}
cat("\n")
}

VI.histogram = function(x){
cat('This is a histogram, with the title:', ifelse(length(x$main)>0, x$main, paste("Histogram of", x$xname)), '\n',
ifelse(length(x$xlab)>0,x$xlab,x$xname), 'is marked on the x-axis.\n')
cat('There are a total of', sum(x$counts), 'elements for this variable.\n')
NoBins=length(x$breaks)-1
if(x$equidist){
cat('It has', NoBins, 'bins with equal widths, starting at', x$breaks[1], 'and ending at', x$breaks[NoBins+1], '.\n')
cat('The mids and counts for the bins are:')
cat(paste0("\nmid = ", x$mids, "  count = ", x$counts))
}
else{
cat('The', NoBins, 'bins have unequal bin sizes.\n')
cat('The intervals and densities for the bins are:')
cat(paste("\nFor the bin from ", x$breaks[1:NoBins], " to ", x$breaks[-1], "the density is ", x$density, sep=""))
}
cat("\n")
}




VI.list=function(x){
cat("No VI method has yet been written for this type of object so it has been printed for you in its entirety.\n")
print(x)
}



VI.data.frame=function(x,...){
ThisDF=x
cat("\nThe summary of each variable is\n")
with(ThisDF,{
for(i in names(ThisDF)){
cat(paste(i,": ", sep=""))
Wanted=summary(get(i))
cat(paste(names(Wanted),Wanted," "))
cat("\n")
}
}) # closure of the with command
cat("\n")
}

VI.matrix=function (x, ...) 
{
    VI.data.frame(as.data.frame.matrix(x), ...)
}

