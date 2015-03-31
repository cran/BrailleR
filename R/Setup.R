# getting some useful batch files for processing R scripts and Rmarkdown files.
# Windows users only

MakeBatch=function(file=NULL){
if(interactive()){
if(version$arch %in% c("i386", "x64")){
RHome = gsub("/", "\\\\", Sys.getenv("R_HOME"))
if(is.null(file)){

# write a batch file for processing R scripts
cat(paste0(RHome, "\\bin\\", version$arch, "\\R.exe CMD BATCH --vanilla --quiet %1\n"), file="RBatch.bat")
cat("RBatch.bat created successfully.\n")
# write a batch file for processing R markdown files
cat(paste0(RHome, "\\bin\\", version$arch, "\\RScript.exe -e \"rmarkdown::render('%1')\"\n"), file="RmdBatch.bat")
cat("RmdBatch.bat created successfully.\n")
cat("These files need to be moved to a folder that is on your system path.\n")
# write a file to show the system path settings
cat(Sys.getenv("PATH"), file="path.txt")
cat("These details are saved in path.txt for reference.\n")

# write a test Rmd file
cat("# a test file
## created by the BrailleR package

My R version is `r version$major`.`r version$minor` and is being used to create this test file
It will then be used to process the test file later once the necessary actions are taken in Windows Explorer.  \n", file="test.Rmd")
cat("test.Rmd created successfully.\n")

# write a test R script
cat("# a test file
## created by the BrailleR package

MySample=sample(100, 10)
MySample
mean(MySample)  \n", file="test.R")
cat("test.R created successfully.\n")
cat("Consult the help page for guidance on using these files in Windows Explorer.\n")
}
else{
FullFile=unlist(strsplit(file, split=".", fixed=TRUE))
if(FullFile[2]=="R"){
# write a batch file for processing the R script
cat(paste0(RHome, "\\bin\\", version$arch, "\\R.exe CMD BATCH --vanilla --quiet ", FullFile[1], ".R\n"), file=paste0(FullFile[1], ".bat"))
cat(paste0(FullFile[1], ".bat created successfully.\n"))
}
if(FullFile[2]=="Rmd"){
# write a batch file for processing the R markdown file
cat(paste0(RHome, "\\bin\\", version$arch, "\\RScript.exe -e \"rmarkdown::render('", FullFile[1], ".Rmd')\"\n"), file=paste0(FullFile[1], ".bat"))
cat(paste0(FullFile[1], ".bat created successfully.\n"))
}
}
}
else{warning("This function is for users running R under the Windows operating system.\n")}
}
else{warning("This function is meant for use in interactive mode only.\n")}
return(invisible(NULL))
}
