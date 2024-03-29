

.BlankMSG =     function() {
message("")
return(invisible(NULL))
}

.Added2MyBrailleR = function(){
message("The installer file has been added to your MyBrailleR folder.")
return(invisible(NULL))
}


.AnswerQuestionsMSG = function(){
message("You will be asked to enter answers for a series of questions.Hit <enter> to use the default shown in parentheses.")
return(invisible(NULL))
}

.AuthorNameMSG =     function() {
message("Enter the name you want to use for authoring content. (",
                getOption("BrailleR.Author"), ")")
return(invisible(NULL))
}

.AutoLoadBrailleR  =     function() {
message("The BrailleR package will be automatically loaded on startup in this working directory.")
return(invisible(NULL))
}





.CanSeeWxPython = function(){
message("Python can see the necessary wx module.")
return(invisible(NULL))
}

.CanUseWriteR  = function(){
message("You are ready to use WriteR.")
return(invisible(NULL))
}

.ConsultHelpPage  =     function() {
message("Consult the help page for guidance on using these files in Windows Explorer.")
return(invisible(NULL))
}

.DefaultSignificanceMSG =     function() {
message("What is the level of significance you plan to use as your default? (",
getOption("BrailleR.SigLevel"), ")")
return(invisible(NULL))
}

.DeleteAnytime  = function(){
message("You can delete it at any time, but that will not uninstall the application.")
return(invisible(NULL))
}

.Done  = function(){
message("Done.")
return(invisible(NULL))
}


.FileCreated =     function(file=NULL, where="in your MyBrailleR directory.") {
NewFile = .ifelse(is.null(file), "A new file", file)
message(NewFile, " has been created ", where)
return(invisible(NULL))
}

.FileUpdated =     function(file=NULL, where="in your MyBrailleR directory.") {
NewFile = .ifelse(is.null(file), "The specified", file)
message(NewFile, " has been updated ", where)
return(invisible(NULL))
}

.GoAdvancedMSG =     function() {
message("By going advanced, you have reduced the verbosity of text descriptions of graphs.")
return(invisible(NULL))
}


.GoBlindMSG =     function() {
message("By going blind, you have turned on the automatic generation of text descriptions of graphs.")
return(invisible(NULL))
}

.GoNoviceMSG =     function() {
message("By going novice, you have returned to receiving all of the automatically generated text descriptions of graphs.")
return(invisible(NULL))
}

.GoSightedMSG =     function() {
message("By going sighted, you have turned off the automatic generation of text descriptions of graphs.")
return(invisible(NULL))
}


.InstallPython =     function() {
message("You could use GetPython3() and GetWxPython3() to help install them.")
return(invisible(NULL))
}


.LatexOffMSG =     function() {
message("You have turned the automatic generation of LaTeX tables off.")
return(invisible(NULL))
}

.LatexOnMSG =     function() {
message("You have turned the automatic generation of LaTeX tables on.")
return(invisible(NULL))
}

.MoveOntoPath=     function() {
message("These files need to be moved to a folder that is on your system path.")
return(invisible(NULL))
}


.NewFile =     function(file=NULL) {
NewFile = .ifelse(is.null(file), "", file)
message(NewFile, " has been created in your working directory.")
return(invisible(NULL))
}

.NoActionTaken =     function() {
message("No action taken.")
return(invisible(NULL))
}



.NoSeePython = function(){
message("Python cannot be seen on your system.If it is installed, then you may need to ensure your system settings are correct.")
return(invisible(NULL))
}

.NothingDoneGraph =     function() {
message("Nothing done to augment this graph object.")
return(invisible(NULL))
}

.NoVIMethod =     function() {
message("There is no specific method written for  this type of object.")
message("You might try to use the print() function on the object or the str() command to investigate its contents.")
return(invisible(NULL))
}


.OptionLocal =     function() {
message("The new setting will remain in effect next time you load the BrailleR package in this directory.")
return(invisible(NULL))
}

.OptionPermanent =     function() {
message("This has overwritten the setting for all folders.")
return(invisible(NULL))
}



.OptionUpdated =     function(option, to=NULL) {
message("The BrailleR.", option, " option has been updated", .ifelse(is.null(to), "", paste0("to ", to )), ".")
return(invisible(NULL))
}


.OriginalDefaults =     function() {
message("You have reset all preferences to the original package defaults.")
return(invisible(NULL))
}



.PValueDigitsMSG =     function() {
message("How many decimal places do you wish p values to be rounded to? (", getOption("BrailleR.PValDigits"), ")")
return(invisible(NULL))
}

.PythonVersion =     function() {
VersionString = system2("python", "--version", stdout=TRUE, stderr=TRUE)
message("Your system is using ", VersionString, "")
return(invisible(NULL))
}

.QuartoVersion =     function() {
VersionString = system2("quarto", "--version", stdout=TRUE, stderr=TRUE)
message("Your system is using ", VersionString, "")
return(invisible(NULL))
}


.SavedInPath =     function() {
message("These details are saved in path.txt for reference.")
return(invisible(NULL))
}





.SVGAndXMLMade =     function() {
message("SVG and XML files created successfully.")
return(invisible(NULL))
}




.UpdatedSettingMSG =     function(What, To) {
message(            "The", What, "has been changed to ", To, "and saved in your settings file.")
return(invisible(NULL))
}


.ViewOffMSG =     function() {
message("You have turned the automatic opening of html pages off.")
return(invisible(NULL))
}

.ViewOnMSG =     function() {
message("You have turned the automatic opening of html pages on.")
return(invisible(NULL))
}
