
boxplot=function(x,...){
    MC <- match.call(expand.dots = TRUE)
    MC[[1L]] <- quote(graphics::boxplot)
names(MC)[2]=""
    Out <- eval(MC, parent.frame())
Out$main = as.character(MC$main)
Out$xlab = as.character(MC$xlab)
Out$ylab = as.character(MC$ylab)
Out$horizontal = as.character(MC$horizontal)
Out$call = MC
class(Out)="boxplot"
return(invisible(Out))
}


hist=function(x,...){
    MC <- match.call(expand.dots = TRUE)
    MC[[1L]] <- quote(graphics::hist)
    Out <- eval(MC, parent.frame())
if(length(MC$main)>0)Out$main = as.character(MC$main)
if(length(MC$xlab)>0)Out$xlab = as.character(MC$xlab)
return(invisible(Out))
}
