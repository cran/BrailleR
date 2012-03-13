txtOut=function(){
Now=date()
Year=substr(Now,21,24)
Month=substr(Now,5,7)
Day=substr(Now,9,10)
Hour=substr(Now,12,13)
Minute=substr(Now,15,16)
Filename=paste("Term",Year,Month,Day, "-",Hour,Minute,".txt",sep="")
CommandSet=paste("Hist",Year,Month,Day,"-",Hour,Minute,".R",sep="")
txtStart(file=Filename, cmdfile=CommandSet)
}
