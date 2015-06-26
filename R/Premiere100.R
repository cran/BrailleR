#' @rdname Embossers
#' @title Prepare BrailleR settings for specific braille embossers
#' @aliases Premiere100
#' @description  Convenience functions for setting package options based on experimentation using specific embossers.
#' @details These functions are only relevant for owners of the specified embossers. Ownership of these models means the user has access to fonts that are licenced to the user. 
#' 
#' The Premiere 100 embosser uses standard 11 by 11.5 inch fanfold braille paper. Printing in landscape or portrait is possible.
#' @return Nothing. The functions are only used to set package options.
#' @seealso \code{\link{ChooseEmbosser}}
#' @author A. Jonathan R. Godfrey.
#' @examples 
#' Premiere100() # Specify use of the Premiere 100 embosser.
#' ChooseEmbosser() # reset to default: using no embosser.

#' @export Premiere100

Premiere100= function(){
ChooseEmbosser("Premiere100")
SetPaperWidth(10, TRUE)
SetPaperHeight(10, TRUE)
SetBRLPointSize(29, TRUE)
return(invisible(NULL))
}
