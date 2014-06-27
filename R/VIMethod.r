

VI = function(x){
UseMethod("VI")
}

VI.default=function(x){
cat("There is no specific method written for  this type of object.\n")
cat("You might try to use the print() function on the object or the str() command to investigate its contents.\n")
print(x)
}


VI.histogram = function(x){
cat(paste("\nThis is a histogram, with", x$xname, " marked on the x-axis, unless you explicitly used the xlab argument.\n"))
cat(paste("There are a total of", sum(x$counts), "elements for this variable.\n"))
NoBins=length(x$breaks)-1
if(x$equidist){
cat(paste("It has ", NoBins, " bins with equal widths, starting at ", x$breaks[1], " and ending at ", x$breaks[NoBins+1], ".\n", sep=""))
cat("The mids and counts for the bins are...")
cat(paste("\nmid = ", x$mids, "  count = ", x$counts, sep=""))
}
else{
cat(paste("The", NoBins, "bins have unequal bin sizes.\n"))
cat("The intervals and densities for the bins are...")
cat(paste("\nFor the bin from ", x$breaks[1:NoBins], " to ", x$breaks[-1], "the density is ", x$density, sep=""))
}
cat("\n\n")
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
}) # closure of hte with command
cat("\n")
}

VI.matrix=function (x, ...) 
{
    VI.data.frame(as.data.frame.matrix(x), ...)
}

