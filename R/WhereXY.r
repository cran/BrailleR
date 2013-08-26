
WhereXY = function(x,y=NULL, grid=c(3,3), Dist="uniform"){

if(is.null(y)){
x=as.matrix(x)
if(length(x[1,])!=2){stop("If y is not supplied, x must have two numeric columns")}
y=x[,2]
if(!is.numeric(y)){stop("This function will fail because y is not numeric")}
x=x[,1]
if(!is.numeric(x)){stop("This function will fail because x is not numeric")}
}
XMin=min(x, na.rm=TRUE)
YMin=min(y, na.rm=TRUE)
XMax=max(x, na.rm=TRUE)
YMax=max(y, na.rm=TRUE)
XRange=XMax-XMin
YRange=YMax-YMin

if(Dist=="uniform"){
XNew=cut(x,breaks=grid[1], labels=FALSE)
YNew=cut(y,breaks=grid[2], labels=FALSE)
}
else{
XMean=mean(x, na.rm=TRUE)
YMean=mean(y, na.rm=TRUE)
XSD=sd(x, na.rm=TRUE)
YSD=sd(y, na.rm=TRUE)
XBreaks=c(0.9*XMin, XMean+XSD*qnorm((1:(grid[1]-1))/grid[1]), 1.1*XMax)
YBreaks=c(0.9*YMin, YMean+YSD*qnorm((1:(grid[2]-1))/grid[2]), 1.1*YMax)
XNew=cut(x, breaks=XBreaks, labels=FALSE)
YNew=cut(y, breaks=YBreaks, labels=FALSE)
}
Output=tapply(x, list(YNew,XNew), length)
return(Output[rev(1:grid[2]),])
}
