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
options(BrailleR.PValDigits=4)
if(interactive()){
chooseCRANmirror(ind=1)
options("menu.graphics"=FALSE)
}
}
