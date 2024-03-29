OneFactor =
    function(Response, Factor, Data = NULL, HSD = TRUE, AlphaE = 0.05,
             Filename = NULL, Folder = NULL, VI = getOption("BrailleR.VI"),
             Latex = getOption("BrailleR.Latex"),
             View = getOption("BrailleR.View"), Modern=TRUE) {


      if (length(Response) == 1) {
        if (is.character(Response)) {
          ResponseName = Response
        }
      } else {
        ResponseName = as.character(match.call()$Response)
      }
      if (length(Factor) == 1) {
        if (is.character(Factor)) {
          FactorName = Factor
        }
      } else {
        FactorName = as.character(match.call()$Factor)
      }



      if (is.null(Data)) {
        Data = data.frame(get(ResponseName), get(FactorName))
        names(Data) = c(ResponseName, FactorName)
        DataName = paste0(ResponseName, ".", FactorName)
        assign(
            paste0(ResponseName, ".", FactorName), Data, envir = parent.frame())
      } else {
        if (length(Data) == 1) {
          if (is.character(Data)) {
            DataName = Data
          }
          Data = get(DataName)
        } else {
          DataName = as.character(match.call()$Data)
        }
        if (!is.data.frame(Data)) .NotADataFrame() 
      }

      with(
          Data,
          {
            if (!is.numeric(get(ResponseName)))
              .ResponseNotNumeric() 
            if (!is.factor(get(FactorName)))
              .GroupNotFactor() 
          })  # end data checking


      # create folder and filenames
      if (is.null(Folder)) Folder = DataName
      if (Folder != "" & !file.exists(Folder)) dir.create(Folder)
      if (is.null(Filename))
        Filename = paste0(.simpleCap(ResponseName), ".", .simpleCap(FactorName),
                          "-OneFactor.Rmd")



      # start writing to the R markdown file
      cat('# Analysis of the', .simpleCap(DataName), 'data, using',
          .simpleCap(ResponseName), 'as the response variable and',
          .simpleCap(FactorName),
          'as the single grouping factor.
#### Prepared by',
          getOption("BrailleR.Author"), '  \n\n', file = Filename)

      cat(paste0(
              '```{r setup, purl=FALSE, include=FALSE}
',
              .ifelse(VI, "library(BrailleR)", ""),
              .ifelse(Modern, "\nlibrary(tidyverse)\nlibrary(ggfortify)", ""),
              '
knitr::opts_chunk$set(dev=c("png", "pdf", "postscript", "svg"))
knitr::opts_chunk$set(echo=FALSE, comment="", fig.path="',
              Folder, '/', .simpleCap(ResponseName), '.',
              .simpleCap(FactorName),
              '-", fig.width=7)
```

<!--- IMPORTANT NOTE: This Rmd file does not yet import the data it uses. 
You will need to add a data import command of some description into the next R chunk to use the file as a stand alone file. --->

```{r importData}
```

## Group summaries

```{r GroupSummary}
attach(',
              DataName, ')
Data.Mean <- tapply(', ResponseName, ', ',
              FactorName, ', mean, na.rm=TRUE)
Data.StDev <- tapply(',
              ResponseName, ', ', FactorName,
              ', sd, na.rm=TRUE)
nNonMissing <- function(x){
length(na.omit(x)) # length() includes NAs
}
Data.n <- tapply(',
              ResponseName, ', ', FactorName,
              ', nNonMissing)
Data.StdErr = Data.StDev/sqrt(Data.n)
detach(',
              DataName,
              ')
DataSummary = data.frame(names(Data.Mean), Data.Mean, Data.StDev, Data.n, Data.StdErr)
colnames(DataSummary) = c("Level", "Mean", "Standard deviation", "n", "Standard error")
```

```{r PrintSummary, results="asis", purl=FALSE}
kable(as.matrix(DataSummary), row.names=FALSE)
```  \n\n'),
          file = Filename, append = TRUE)




      cat("The ratio of the largest group standard deviation to the smallest is `r round(max(Data.StDev)/min(Data.StDev),2)`  \n\n",
          file = Filename, append = TRUE)

      nNonMissing <- function(x) {
        length(na.omit(x))  # length() includes NAs
      }
      Data.n <- with(get(DataName),
                     tapply(get(ResponseName), get(FactorName), nNonMissing))

      if (min(Data.n) > 4) {
        cat(paste0('## Comparative boxplots 

```{r boxplots, fig.cap="Comparative boxplots"}  
            ',
                .ifelse(VI, "VI(", ""), 'boxplot(', ResponseName, '~',
                FactorName, ', data=', DataName, ', ylab=',
                InQuotes(.simpleCap(ResponseName)), ', xlab=',
                InQuotes(.simpleCap(FactorName)), .ifelse(VI, ")", ""),
 ')
``` \n\n'), file = Filename, append = TRUE)
      }
        else {
        cat('## Comparative boxplots  \n
No boxplots are included because at least one group size is too small.  \n\n',
            file = Filename, append = TRUE)
      }

      cat(paste0(
              '## Comparative dotplots

```{r dotplots, fig.cap="Comparative dotplots"}
with(',
              DataName, ',
', .ifelse(VI, 'VI(dotplot(', 'stripchart('),
              ResponseName, '~', FactorName, ', xlab=',
              InQuotes(.simpleCap(ResponseName)), ', ylab=',
              InQuotes(.simpleCap(FactorName)), .ifelse(VI, ')', ''),
              '))
``` \n\n'), file = Filename, append = TRUE)

      cat(paste0('## One-way Analysis of Variance

```{r OneWayANOVA1}
',
                 .ifelse(VI, "VI(", ""), '
MyANOVA <- aov(', ResponseName, '~',
                 FactorName, ', data=', DataName, ')', .ifelse(VI, ")", "")),
          '
summary(MyANOVA)
```  \n\n', file = Filename, append = TRUE)

      cat('\n\n## Residual Analysis\n\n', file = Filename, append = TRUE)

ResidualText = .ifelse(Modern, .GetModernStyleResidualText(ModelName="MyANOVA"), .GetOldStyleResidualText(ModelName="MyANOVA"))
      cat(ResidualText, file = Filename, append = TRUE)

        cat(paste0(
                '\n\n## Tests for homogeneity of Variance

```{r BartlettTest}
bartlett.test(',
              ResponseName, '~', FactorName, ', data=', DataName,
              ')
```

```{r FlignerTest}
fligner.test(', ResponseName, '~',
              FactorName, ', data=', DataName, ')
``` \n\n'), file = Filename,
          append = TRUE)



      if (HSD) {
        cat(paste0(
                '## Tukey Honestly Significant Difference test

```{r TukeyHSD, fig.cap="Plot of Tukey HSD"}
MyHSD <- TukeyHSD(MyANOVA, ordered=TRUE, conf.level=',
                1 - AlphaE, ')
', .ifelse(VI, "VI(MyHSD)  \n", ""),
                '
MyHSD
plot(MyHSD)
``` \n\n'), file = Filename,
            append = TRUE)
      }

      if (Latex) {
        cat(paste0(
                '## Tables for LaTeX documents

N.B. Set `Latex=FALSE` to stop creation of these tables in future

```{r DataSummaryTex, purl=FALSE}
library(xtable)
ThisTexFile = "',
                Folder, '/', .simpleCap(ResponseName), '.',
                .simpleCap(FactorName),
                '-GroupSummary.tex"
TabCapt = "Summary statistics for ',
                .simpleCap(ResponseName), ' by level of ',
                .simpleCap(FactorName),
                '"
print(xtable(DataSummary, caption=TabCapt, label="',
                ResponseName,
                'GroupSummary", digits=4, align="llrrrr"), include.rownames = FALSE, file = ThisTexFile)
```  

```{r ANOVA-TEX, purl=FALSE}
ThisTexFile = "',
                Folder, '/', .simpleCap(ResponseName), '-',
                .simpleCap(FactorName),
                '-ANOVA.tex"
TabCapt = "One-way ANOVA for ',
                .simpleCap(ResponseName), ' with the group factor ',
                .simpleCap(FactorName),
                '."
print(xtable(MyANOVA, caption=TabCapt, label="',
                .simpleCap(ResponseName), '-', .simpleCap(FactorName),
                '-ANOVA", digits=4), file = ThisTexFile)
```  \n\n'),
            file = Filename, append = TRUE)
      }



      # finish writing markdown and process the written file into html and an R script
      knit2html(Filename, quiet = TRUE,
                meta = list(css = FindCSSFile(getOption("BrailleR.Style"))))
      file.remove(sub(".Rmd", ".md", Filename))
      purl(Filename, quiet = TRUE, documentation = 0)
      if (View) browseURL(sub(".Rmd", ".html", Filename))
    }  # end of OneFactor function
