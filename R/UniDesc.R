
UniDesc = function(Response=NULL, ResponseName=as.character(match.call()$Response), Basic = TRUE, Graphs = TRUE, Normality=TRUE, Tests=TRUE, Filename=NULL, Folder=ResponseName, VI=TRUE, Latex=TRUE, View=TRUE){

if(is.null(Response)){
if(length(ResponseName)==0) stop("You must specify either the Response or the ResponseName.")
Response=get(ResponseName)
}

# to ensure consistent and predictable appearance
StartChunk=function(ChunkName, ThisFile=Filename){cat('\n```{r', ChunkName, '}   \n', file=ThisFile, append=TRUE)}
CloseChunk=function(ThisFile=Filename){cat('\n```   \n\n', file=ThisFile, append=TRUE)}
GraphHead=function(GraphName, AltTag, ThisFile=Filename){cat(paste0('\n```{r ', GraphName, ', fig.cap="', AltTag, '", fig.height=5}    \n'), file=ThisFile, append=TRUE)}
GraphHeadSq=function(GraphName, AltTag, ThisFile=Filename){cat(paste0('\n```{r ', GraphName, ', fig.cap="', AltTag, '", fig.height=7}  \n'), file=ThisFile, append=TRUE)}
GraphHeadWide=function(GraphName, AltTag, ThisFile=Filename){cat(paste0('\n```{r ', GraphName, ', fig.cap="', AltTag, '", fig.height=3.5}  \n'), file=ThisFile, append=TRUE)}

# to avoid errors for missing folders
if(Graphs || Latex){
if(Folder!="."&& !file.exists(Folder)) dir.create(Folder)
}

# make sure there is a filename to write the markdown to.
if(is.null(Filename)){
Filename = paste0(ResponseName, "-UniDesc.Rmd")
}


# Start writing mark down here.
cat('\n# Univariate analysis for', ResponseName, '\n', file=Filename)
cat(paste0('```{r setup, include=FALSE}\nopts_chunk$set(dev=c("png", "pdf", "postscript", "svg"))\nopts_chunk$set(comment="", echo=FALSE, fig.path="', Folder, '/', ResponseName, '-", fig.width=7)\n```    \n'), file=Filename, append=TRUE)

#paste0('fig.path="', Folder, '/', ResponseName, '-"'),


if(Basic){
cat("\n## Basic summary measures    \n### Counts    \n", 
paste0("`r length(", ResponseName, ")`"), "values in all, made up of  ",
paste0("\n`r length(unique(", ResponseName, "))`"), "unique values,  ",
paste0("\n`r sum(!is.na(", ResponseName, "))`"), "observed, and  ",
paste0("\n`r sum(is.na(", ResponseName, "))`"), "missing values.   \n", 
file=Filename, append=TRUE)

cat("\n### Measures of location    \n\n", 
"Mean | Value  \n",
"----- | ------  \n",
paste0("All data |  `r mean(", ResponseName, ", na.rm = TRUE)`    \n"), 
paste0("Trimmed 5% | `r mean(", ResponseName, ", trim =0.025, na.rm = TRUE)`    \n"), 
paste0("Trimmed 10% | `r mean(", ResponseName, ", trim =0.05, na.rm = TRUE)`    \n"), 
file=Filename, append=TRUE)


cat("\n### Quantiles    \n",
'```{r Quantiles1}  \n',
paste0('Quantiles=quantile(', ResponseName, ', na.rm=TRUE)\n'),
'QList=c("Minimum", "Lower Quartile", "Median", "Upper Quartile", "Maximum")  
Results=data.frame(Quantile=QList, Value=Quantiles[1:5])  
```  

```{r Quantiles2, results="asis", purl=FALSE}  
kable(Results) \n```  \n', file=Filename, append=TRUE)

cat("\n### Measures of spread    \n", file=Filename, append=TRUE)
cat(paste0("Measure | Value  
-------- | ------  
IQR | `r IQR(", ResponseName, ", na.rm = TRUE)`    \n"), 
paste0("Standard deviation | `r sd(", ResponseName, ", na.rm = TRUE)`   \n"), 
paste0("Variance | `r var(", ResponseName, ", na.rm = TRUE)`   \n"), file=Filename, append=TRUE)

cat("\n### Other moments     \n", file=Filename, append=TRUE)
cat(paste0("Moment | Value  
-------- | ------  
Skewness | `r moments::skewness(", ResponseName, ", na.rm = TRUE)`    \n"), file=Filename, append=TRUE)
cat(paste0("Kurtosis | `r moments::kurtosis(", ResponseName, ", na.rm = TRUE)`   \n"), file=Filename, append=TRUE)
}

if(Graphs){
cat("\n## Basic univariate graphs    \n### Histogram    \n", file=Filename, append=TRUE)
GraphHead("Hist", "The histogram")
cat(paste0(ifelse(VI, 'VI(',''), 'hist(', ResponseName, ', xlab="', ResponseName, '", main="Histogram of ', ResponseName, '")', ifelse(VI, ')', '') ), file=Filename, append=TRUE)
CloseChunk()

#cat(paste0("The title put on this histogram was: Histogram of",ResponseName,"\n"))
#cat(paste0("The x-axis for this histogram was given the label:",ResponseName,"\n"))

cat("\n### Boxplot    \n", file=Filename, append=TRUE)
GraphHeadWide("Boxplot", "The boxplot")
cat(paste0(ifelse(VI, 'VI(',''), 'boxplot(', ResponseName, ', horizontal=TRUE, main = "Boxplot of ', ResponseName, '")', ifelse(VI, ')','')), file=Filename, append=TRUE)
CloseChunk()

#cat("\n### Density plot    \n", file=Filename, append=TRUE)
#GraphHead("Density")
#cat(paste0('density(', ResponseName, ', na.rm=TRUE, main = "Density plot for ', ResponseName, '")'), file=Filename, a#ppend=TRUE)
#CloseChunk()
#MyDensity=plot(density(x, xlab=ResponseName)
#cat(paste0("The title put on this density plot was: Density plot of",ResponseName,"\n"))
#cat(paste0("The x-axis for this density plot was given the label:",ResponseName,"\n"))
#VI(MyDensity)

}# end of basic graphs section

if(Normality){
cat("\n## Assessing normality    \n", file=Filename, append=TRUE)

cat('\n### Formal tests for normality    \n', file=Filename, append=TRUE)

cat('```{r NormalityTests, results="asis"}\nResults = matrix(0, nrow=6, ncol=2)
dimnames(Results) = list(c("Shapiro-Wilk", "Anderson-Darling", "Cramer-von Mises", "Lilliefors (Kolmogorov-Smirnov)", "Pearson chi-square", "Shapiro-Francia"),
 c("Statistic", "P Value"))
 SW =shapiro.test(', ResponseName, ')
Results[1,] = c(SW$statistic, SW$p.value)
AD = ad.test(', ResponseName, ')
Results[2,] = c(AD$statistic, AD$p.value)
CV = cvm.test(', ResponseName, ')
Results[3,] = c(CV$statistic, CV$p.value)
LI = lillie.test(', ResponseName, ')
Results[4,] = c(LI$statistic, LI$p.value)
PE = pearson.test(', ResponseName, ')
Results[5,] = c(PE$statistic, PE$p.value)
SF = sf.test(', ResponseName, ')
Results[6,] = c(SF$statistic, SF$p.value)
', paste0('ThisTexFile = "', Folder, "/", ResponseName, '-Normality.tex"'),'
TabCapt= "Tests for normality: Variable is', ResponseName, '"
print(xtable(Results, caption=TabCapt, label=paste0(ResponseName,"Normality"), digits=4, align="lrr"), file = ThisTexFile)
kable(Results)    \n```   \n', file=Filename, append=TRUE)
}# end of normality test statistics section

if(Graphs){
cat("\n### Normality plot    \n", file=Filename, append=TRUE)
GraphHeadSq("NormPlot", "The normality plot")
cat(paste0('qqnorm(', ResponseName, ', main = "Normality Plot for ', ResponseName, '")\nqqline(', ResponseName, ')'), file=Filename, append=TRUE)
CloseChunk()
}# end of normality plot section

if(Tests){
cat('\n## Formal tests of moments    \n', file=Filename, append=TRUE)

cat('```{r MomentsTests, results="asis"}\nResults = matrix(0, nrow=2, ncol=3)
dimnames(Results)= list(c( "D\'Agostino skewness", "Anscombe-Glynn kurtosis"), 
 c("Statistic","Z",  "P Value"))
AG = moments::agostino.test(', ResponseName, ')
AN = moments::anscombe.test(', ResponseName, ')
Results[1,] = c(AG$statistic, AG$p.value)
Results[2,] = c(AN$statistic, AN$p.value)
',paste0('ThisTexFile = "', Folder, "/", ResponseName, '-Moments.tex"'),'
TabCapt= "Tests on moments: Variable is', ResponseName, '"
print(xtable(Results, caption=TabCapt, label=', paste0('\"', ResponseName, 'Moments\"'), ', digits=4, align=\"lrrr\"), file = ThisTexFile)
kable(Results)    \n```   \n', file=Filename, append=TRUE)
}# end of formal tests of normality via moments section

# stop writing markdown and process the written file into html and an R script
knit2html(Filename, quiet=TRUE)
file.remove(sub(".Rmd", ".md", Filename))
purl(Filename, quiet=TRUE)
if(View) browseURL(sub(".Rmd", ".html", Filename))
}# end of UniDesc function definition
