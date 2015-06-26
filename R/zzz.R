.onAttach=function(libname, pkgname){
if(interactive()){
packageStartupMessage("The BrailleR.View option has been set to TRUE. \nConsult the help page for GoSighted() to see how settings can be altered.\nYou may wish to use the GetGoing() function as a quick way of getting started.")
}
else{
packageStartupMessage("The BrailleR.View,  option is set to FALSE.")
}
}

.onLoad=function(libname, pkgname){
if(system.file("PREFERENCES", package="BrailleR")==""){# first time package is used
    DefSettings=system.file("DEFAULTS", package="BrailleR")
    PrefSettings=sub("DEFAULTS", "PREFERENCES", DefSettings)
    file.copy(DefSettings, PrefSettings)}
options(BrailleR.View=interactive())
OpSet=read.dcf(system.file("PREFERENCES", package="BrailleR"), all=TRUE)
OpSet$BrailleR.SigLevel=as.numeric(OpSet$BrailleR.SigLevel)
OpSet$BrailleR.PValDigits=as.numeric(OpSet$BrailleR.PValDigits)
OpSet$BrailleR.DotplotBins=as.numeric(OpSet$BrailleR.DotplotBins)
OpSet$BrailleR.BRLPointSize = as.numeric(OpSet$BrailleR.BRLPointSize)
OpSet$BrailleR.PaperWidth = as.numeric(OpSet$BrailleR.PaperWidth)
OpSet$BrailleR.PaperHeight = as.numeric(OpSet$BrailleR.PaperHeight)
#OpSet$ = as.numeric(OpSet$)
do.call(options, as.list(OpSet))
BrailleR=new.env(parent=.GlobalEnv)
if(interactive()){
utils::chooseCRANmirror(ind=1)
options("menu.graphics"=FALSE)
}
}
