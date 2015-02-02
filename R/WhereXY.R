
WhereXY = function(x,y=NULL, grid=c(4,4), xDist="uniform", yDist=xDist, addmargins=TRUE){

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

if(xDist=="uniform"){
XNew=cut(x,breaks=grid[1], labels=FALSE)}
else{
XMean=mean(x, na.rm=TRUE)
XSD=sd(x, na.rm=TRUE)
XBreaks=c(0.9*XMin, XMean+XSD*qnorm((1:(grid[1]-1))/grid[1]), 1.1*XMax)
XNew=cut(x, breaks=XBreaks, labels=FALSE)
}

if(yDist=="uniform"){
YNew=cut(y,breaks=grid[2], labels=FALSE)}
else{
YMean=mean(y, na.rm=TRUE)
YSD=sd(y, na.rm=TRUE)
YBreaks=c(0.9*YMin, YMean+YSD*qnorm((1:(grid[2]-1))/grid[2]), 1.1*YMax)
YNew=cut(y, breaks=YBreaks, labels=FALSE)
}
XNew=paste0("(", XNew, ")")
YNew=paste0("(", YNew, ")")
Output=tapply(x, list(YNew,XNew), length)
Output[is.na(Output)]=0
Output=Output[rev(1:grid[2]),]
if(addmargins){
Output=addmargins(Output)
}
return(Output)
}
