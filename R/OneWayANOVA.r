# only fault is the fit quoted in the TukeyHSD part.

OneWayANOVA = function(DataName, ResponseName, FactorName, HSD=TRUE, AlphaE=0.05, Folder="", VI=TRUE){

if(Folder==""){
Folder=DataName
if(!file.exists(Folder)) dir.create(Folder)
}
FullFile = paste(Folder, "\\", ResponseName,"-", FactorName, "ANOVA.txt", sep="")


ThisData=get(DataName)
attach(ThisData)
on.exit(detach(ThisData)) 
Factor =as.factor( get(FactorName ))
Response = as.numeric(get(ResponseName))

ThisData[,ResponseName] = as.numeric(ThisData[,ResponseName])
ThisData[,FactorName] = as.factor(ThisData[,FactorName])

if(VI){
sink(FullFile, append=TRUE)
}
cat(paste("Analysis of variance for the", DataName, "data, using", ResponseName, "as the response variable and", FactorName, "as the single factor variable.\n\n", sep=" "))

Data.sd <- tapply(Response, Factor, sd, na.rm=TRUE)
cat("The standard deviations for the treatment groups are\n")
print(Data.sd, digits=5)
cat(paste("The ratio of the largest treatment group standard deviation to the smallest is ",round(max(Data.sd)/min(Data.sd),2),"\n\n\n"))

cat(paste("Analysis of variance table for ",ResponseName,"\n\n", sep=""))
ModelForm = as.formula(paste(ResponseName,"~",FactorName, sep=""))
Data.aov <- aov(ModelForm, data=ThisData)
print(summary(Data.aov), digits=4)
cat("\n\n")

print(model.tables(Data.aov, type="means", se=TRUE))
cat("\n\n")

par(mfrow=c(2,2))
plot(Data.aov)

if(HSD){
Data.hsd <- TukeyHSD(Data.aov, FactorName, ordered=TRUE, conf.level=1-AlphaE)
print(Data.hsd, digits=4)
par(mfrow=c(1,1))
plot(Data.hsd)
}
if(VI){
sink()
}
return(Data.aov)
}

# and some examples
#OneWayANOVA("InsectSprays", "count", "spray")
#OneWayANOVA("airquality", "Ozone", "Month")
