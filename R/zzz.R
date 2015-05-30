.onAttach=function(libname, pkgname){
if(interactive()){
packageStartupMessage("The BrailleR.View option has been set to TRUE. \nConsult the help page for GoSighted() to see how settings can be altered.\nYou may wish to use the GetGoing() function as a quick way of getting started.")
}
else{
packageStartupMessage("The BrailleR.View,  option is set to FALSE.")
}
}

.onLoad=function(libname, pkgname){
options(BrailleR.View=interactive())
OpSet=read.dcf(system.file("PREFERENCES", package="BrailleR"), all=TRUE)
OpSet$BrailleR.SigLevel=as.numeric(OpSet$BrailleR.SigLevel)
OpSet$BrailleR.PValDigits=as.numeric(OpSet$BrailleR.PValDigits)
OpSet$BrailleR.DotplotBins=as.numeric(OpSet$BrailleR.DotplotBins)
do.call(options, as.list(OpSet))
BrailleR=new.env(parent=.GlobalEnv)
if(interactive()){
chooseCRANmirror(ind=1)
options("menu.graphics"=FALSE)
}
}
