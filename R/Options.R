# functions for setting the package options easily

GoSighted = function(){
options(BrailleR.VI=FALSE)
}

GoBlind = function(){
options(BrailleR.VI=TRUE)
}

ViewOn = function(){
options(BrailleR.View=TRUE)
}

ViewOff = function(){
options(BrailleR.View=FALSE)
}

LatexOn = function(){
options(BrailleR.Latex=TRUE)
}

LatexOff = function(){
options(BrailleR.Latex=FALSE)
}
