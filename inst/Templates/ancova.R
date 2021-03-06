plot(ResponseName~Covariate, data=DataName, col=FactorName)
DataName.aov1 = aov(ResponseName ~ FactorName, data=DataName)
DataName.aov2 = aov(ResponseName ~ FactorName+Covariate, data=DataName)
DataName.aov3 = aov(ResponseName ~ FactorName*Covariate, data=DataName)
anova(DataName.aov1, DataName.aov2, DataName.aov3)
