GetGoing = function(){
if(interactive()){
cat("You will be asked to enter answers for a series of questions.\nHit <enter> to use the default shown in parentheses.\n")

cat(paste0("\nEnter the name you want to use for authoring content. (", getOption("BrailleR.Author"), ")\n"))
name = readLines(n=1)
if(name!="") SetAuthor(name)

cat(paste0("\nHow many decimal places do you wish p values to be rounded to? (", getOption("BrailleR.PValDigits"), ")\n"))
digits = as.numeric(readLines(n=1))
if(!is.na(digits)) SetSigLevel(digits)

cat(paste0("\nWhat is the level of significance you plan to use as your default? (", getOption("BrailleR.SigLevel"), ")\n"))
alpha = as.numeric(readLines(n=1))
if(!is.na(alpha)) SetSigLevel(alpha)

cat("\nThe following questions are yes/no questions. Use T or TRUE for yes, F or FALSE for no.\n")

cat("\nDo you want to process R scripts and Rmd files outside R? (TRUE)\n")
batch  = as.logical(readLines(n=1))
if(is.na(batch)) batch=TRUE
if(batch) MakeBatch()

cat("\nDo you want to incorporate output from R into LaTeX files? (TRUE)\n")
latex  = as.logical(readLines(n=1))
if(is.na(latex)) latex=TRUE
if(latex){LatexOn()}
else{LatexOff()}

cat("\nDo you want to automatically open HTML files of R output? (TRUE)\n")
view  = as.logical(readLines(n=1))
if(is.na(view)) view=TRUE
if(view){ViewOn()}
else{ViewOff()}

cat("\nBrailleR assumes you are blind. Is this how you will work? (TRUE)\n")
vi  = as.logical(readLines(n=1))
if(is.na(vi)) vi=TRUE
if(vi){GoBlind()}
else{GoSighted()}


#end interactive section.
}
else{warning("This function is only intended for interactive R sessions.\n")}
return(invisible(NULL))
}
