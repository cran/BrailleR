# Getting started with the WriteR application
# only for Windows users at present.

PrepareWriteR = function(Author=getOption("BrailleR.Author")){
if(interactive()){
if(version$arch %in% c("i386", "x64")){
RHome= gsub("/", "\\\\\\\\", Sys.getenv("R_HOME"))
MyHome = gsub("/", "\\\\\\\\", gsub("\\\\", "/", Sys.getenv("HOME")))

cat(paste0('{
    "RDirectory": "', RHome, '\\\\bin\\\\', version$arch, '\\\\Rscript.exe",
    "buildcommand": "rmarkdown::render(\"{}\")",
    "dirname": "', MyHome, '",
    "filename": "untitled.Rmd",
    "newText": "# \\n## by ', Author, ' on \\n\\n",
    "repo": "http://cran.stat.auckland.ac.nz/"
}
'), file='WriteR.init')

cat("The settings file for WriteR has been created.\n")

file.copy(paste0(system.file(package="BrailleR"), "/Python/Writer/", c("WriteR.pyw", "HelpPage.html", "HelpPage.Rmd", "Basics.Rmd")), ".")
cat("Copies of the main wxPython script and help page documents have been copied \ninto your working directory.\n")
cat("You can move these files to your preferred folder for WriteR, \nor start working here.\n")
}
else{warning("This function is for users running R under the Windows operating system.\n")}
}
else{warning("This function is meant for use in interactive mode only.\n")}
return(invisible(NULL))
}

