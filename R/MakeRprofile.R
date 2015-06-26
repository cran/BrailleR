MakeRprofile = function(Overwrite=FALSE){
if(!file.exists(".Rprofile")) Overwrite=TRUE
if(Overwrite){
cat(".First=function(){\n  library(BrailleR)\n}\n", file=".Rprofile")
cat("The .Rprofile file has been updated. The BrailleR package will be automatically loaded on startup in this working directory.\n")
}
else{
warning("An .Rprofile already exists. No action has been taken.\n")
}
return(invisible(NULL))
}
