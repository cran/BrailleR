.onAttach=function(libname, pkgname){
if(interactive()){
packageStartupMessage("Options BrailleR.VI, BrailleR.View, and BrailleR.Latex have all been set to TRUE. \nConsult the help page for GoSighted() to see how these settings can be altered.")
}
else{
packageStartupMessage("Options BrailleR.VI and BrailleR.Latex have been set to TRUE, but BrailleR.View,  is set to FALSE.")
}
}

.onLoad=function(libname, pkgname){
options(BrailleR.VI=TRUE)
options(BrailleR.View=interactive())
options(BrailleR.Latex=TRUE)
options(BrailleR.SigLevel=0.05)
options(BrailleR.PValDigits=4)
options(BrailleR.Author=readLines(system.file("Settings", "Author.txt", package="BrailleR"), warn=FALSE))
BrailleR=new.env(parent=.GlobalEnv)
if(interactive()){
chooseCRANmirror(ind=1)
options("menu.graphics"=FALSE)
}
}
