txtOut=function(Filename=NULL){
if(is.null(Filename)){
cat("Please enter a filename and press <enter>. \nPressing <enter> alone will generate a default filename based on the current date and time.\n")
Filename=scan(,what=character(0), quiet=TRUE)
Filename=paste(Filename[1],"txt",sep=".")
CommandSet=paste(Filename[1],"R",sep=".")
}
else{
CommandSet = paste(sub(".txt", "", Filename), "R", sep=".")
}
if(is.na(Filename[1])){
Now=date()
Year=substr(Now,21,24)
Month=substr(Now,5,7)
Day=substr(Now,9,10)
Hour=substr(Now,12,13)
Minute=substr(Now,15,16)
Filename=paste("Term",Year,Month,Day, "-",Hour,Minute,".txt",sep="")
CommandSet=paste("Hist",Year,Month,Day,"-",Hour,Minute,".R",sep="")
}
txtStart(file=Filename, cmdfile=CommandSet)
}
