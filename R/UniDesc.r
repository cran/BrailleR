
UniDesc = function(ResponseName, Response=NULL, Basic = TRUE, Graphs = TRUE, Normality=TRUE, Tests=TRUE, Folder="", VI=TRUE){


if(!Folder==""){
if (!file.exists(Folder)) dir.create(Folder)
}

if(is.null(Response)){
x=get(ResponseName)}
else{ x= Response}
x = as.vector(x)
x= as.numeric(x)

FullFile = paste(Folder, "\\", ResponseName, "Desc.txt", sep="")

cat(paste("Univariate analysis for ", ResponseName, "\n", sep=""), file=FullFile)

if(Basic){
CountMiss = sum(is.na(x))
cat(paste("\nCount Nonmissing: ", length(x)- CountMiss, sep=""), file=FullFile, append=TRUE)
cat(paste("\nCount Missing: ", CountMiss, "\n", sep=""), file=FullFile, append=TRUE)
cat(paste("\nMean: ", mean(x, na.rm = TRUE), sep=""), file=FullFile, append=TRUE)
cat(paste("\nTrimmed mean(5%): ", mean(x, trim =0.025, na.rm = TRUE), sep=""), file=FullFile, append=TRUE)
cat(paste("\nStandard deviation: ", sd(x, na.rm = TRUE),"\n",  sep=""), file=FullFile, append=TRUE)


Q= quantile(x, na.rm = TRUE)
cat(paste("\nMinimum: ", Q[1], sep=""), file=FullFile, append=TRUE)
cat(paste("\nLower: ", Q[2], sep=""), file=FullFile, append=TRUE)
cat(paste("\nMedian: ", Q[3], sep=""), file=FullFile, append=TRUE)
cat(paste("\nUpper: ", Q[4], sep=""), file=FullFile, append=TRUE)
cat(paste("\nMaximum: ", Q[5], sep=""), file=FullFile, append=TRUE)
cat(paste("\nIQR: ",IQR(x, na.rm = TRUE), "\n", sep=""), file=FullFile, append=TRUE)
}

if(Graphs){
MyHist=hist(x, xlab=ResponseName, main=paste("Histogram of ",ResponseName, sep=""))
dev.copy2eps(file = paste(Folder, "\\", ResponseName, "-Hist.eps", sep=""))
dev.copy2eps(file = paste(Folder, "\\", ResponseName, "-Hist.pdf", sep=""))
dev.off()
if(VI){
sink(FullFile, append=TRUE)
cat("\n")
cat(paste("The title put on this histogram was: Histogram of",ResponseName,"\n"))
cat(paste("The x-axis for this histogram was given the label:",ResponseName,"\n"))
VI(MyHist)
sink()
cat("\n")
}

MyDensity=plot(density(x, na.rm=TRUE), xlab=ResponseName, main=paste("Density plot for ", ResponseName, sep=""))
dev.copy2eps(file = paste(Folder, "\\", ResponseName, "-Density.eps", sep=""))
dev.copy2eps(file = paste(Folder, "\\", ResponseName, "-Density.pdf", sep=""))
dev.off()
if(VI){
sink(FullFile, append=TRUE)
cat("\n")
cat(paste("The title put on this density plot was: Density plot of",ResponseName,"\n"))
cat(paste("The x-axis for this density plot was given the label:",ResponseName,"\n"))
VI(MyDensity)
cat("\n")
sink()
}
}

if(Normality){
Results = matrix(0, nrow=6, ncol=2)
dimnames(Results)[[2]] = c("Statistic", "P Value")
dimnames(Results)[[1]] = c("Shapiro-Wilk", "Anderson-Darling", "Cramer-von Mises", "Lilliefors (Kolmogorov-Smirnov)", "Pearson chi-square", "Shapiro-Francia")
 SW =shapiro.test(x)
Results[1,] = c(SW$statistic, SW$p.value)
AD = ad.test(x)
Results[2,] = c(AD$statistic, AD$p.value)
CV = cvm.test(x)
Results[3,] = c(CV$statistic, CV$p.value)
LI = lillie.test(x)
Results[4,] = c(LI$statistic, LI$p.value)
PE = pearson.test(x)
Results[5,] = c(PE$statistic, PE$p.value)
SF = sf.test(x)
Results[6,] = c(SF$statistic, SF$p.value)
FileName = paste(Folder, "\\", ResponseName,"-Normality.tex", sep="")
TabCapt = paste("Tests for normality: Variable is ", ResponseName, sep="")
print(xtable(Results, caption=TabCapt, label=paste(ResponseName,"Normality", sep=""), digits=4, align="lrr"), file = FileName)
}

if(Graphs){
qqnorm(x, main = paste("Normality Plot for ",ResponseName, sep=""))
 qqline(x)
dev.copy2eps(file = paste(Folder, "\\", ResponseName, "-NormPlot.eps", sep=""))
dev.copy2eps(file = paste(Folder, "\\", ResponseName, "-NormPlot.pdf", sep=""))
dev.off()
}

if(Tests){
Results = matrix(0, nrow=2, ncol=3)
dimnames(Results)[[2]] = c("Statistic","Z",  "P Value")
dimnames(Results)[[1]] = c( "D'Agostino skewness", "Anscombe-Glynn kurtosis")
AG = agostino.test(x)
AN = anscombe.test(x)
Results[1,] = c(AG$statistic, AG$p.value)
Results[2,] = c(AN$statistic, AN$p.value)

FileName = paste(Folder, "\\", ResponseName,"-Moments.tex", sep="")
TabCapt= paste("Tests on moments: Variable is ", ResponseName, sep="")
print(xtable(Results, caption=TabCapt, label=paste(ResponseName,"Moments", sep=""), digits=4, align="lrrr"), file = FileName)
}
}
