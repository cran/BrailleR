# functions for setting the package options as easily as possible. Most do not have arguments.

# these ones do have arguments

SetAuthor=function(name="BrailleR", Permanent=TRUE){
if(is.character(name)){
options(BrailleR.Author=name)
cat(paste0("The BrailleR.Author option has been updated to ", name, ".\n"))
if(Permanent){ cat(name, file=system.file("Settings", "Author.txt", package="BrailleR"))
cat("and will remain in effect next time you load the package.\n")}
}
else{warning("A text string was expected. No action taken.\n")}
return(invisible(NULL))
}

SetSigLevel=function(alpha){
if((0<alpha)&(alpha<1)){
options(BrailleR.SigLevel=alpha)
cat(paste0("The BrailleR.SigLevel option for the level of alpha has been changed to ", alpha, ".\n"))
}
else{
warning("The level of alpha must be between 0 and 1. \nNo change has been made to this setting.\n")
}
return(invisible(NULL))
}

# functions below this point have no arguments.

GoSighted = function(){
options(BrailleR.VI=FALSE)
cat("By going sighted, you have turned off the automatic generation of text descriptions of graphs.\n")
return(invisible(NULL))
}

GoBlind = function(){
options(BrailleR.VI=TRUE)
cat("By going blind, you have turned on the automatic generation of text descriptions of graphs.\n")
return(invisible(NULL))
}

ViewOn = function(){
options(BrailleR.View=TRUE)
cat("You have turned the automatic opening of html pages on.\n.\n")
return(invisible(NULL))
}

ViewOff = function(){
options(BrailleR.View=FALSE)
cat("You have turned the automatic opening of html pages on.\ff.\n")
return(invisible(NULL))
}

LatexOn = function(){
options(BrailleR.Latex=TRUE)
cat("You have turned the automatic generation of LaTeX tables on.\n")
return(invisible(NULL))
}

LatexOff = function(){
options(BrailleR.Latex=FALSE)
cat("You have turned the automatic generation of LaTeX tables off.\n")
return(invisible(NULL))
}
