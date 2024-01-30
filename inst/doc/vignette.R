## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  
#  install.packages("MplusAutomation")
#  

## -----------------------------------------------------------------------------

library(MplusAutomation)


## -----------------------------------------------------------------------------

sessionInfo()


## ----eval=FALSE---------------------------------------------------------------
#  
#  update.packages(ask=FALSE, checkBuilt=TRUE)
#  

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  
#  prepareMplusData(
#    my_data,
#    filename = "C:/Data_Analysis/Prepare Mplus.dat",
#    keepCols=c("id", "item1", "item3", "item6"))
#  
#  prepareMplusData(
#    my_other_data,
#    filename = "C:/Data_Analysis/Prepare Dropped Mplus.dat",
#    dropCols=c("baditem1", "baditem2", "baditem7"))
#  

## -----------------------------------------------------------------------------
data(mtcars)
mtcars$gear <- factor(mtcars$gear)
prepareMplusData(mtcars, "mtcars.dat", dummyCode = c("cyl", "am"))

## ----eval=FALSE, echo = TRUE--------------------------------------------------
#  
#  runModels("C:/Program Files/Mplus/Mplus Examples/Addendum Examples")
#  

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  
#  runModels(
#    "C:/Data_Analysis/ComparingLCAvCFA",
#    recursive=TRUE)
#  

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  
#  runModels(
#    "C:/Data_Analysis/ComparingLCAvCFA",
#    recursive=TRUE,
#    logFile="C:/CFALCA-Comparison-Log.txt")|
#  

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  
#  runModels(
#    "C:/Data_Analysis/ComparingLCAvCFA",
#    recursive=TRUE,
#    logFile=NULL)
#  

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  
#  runModels(
#    "C:/Data_Analysis/ComparingLCAvCFA",
#    recursive=TRUE,
#    replaceOutfile="never")
#  

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  
#  runModels(
#    "C:/Data_Analysis/ComparingLCAvCFA",
#    recursive=TRUE,
#    replaceOutfile="modifiedDate")
#  

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  
#  runModels(
#    "C:/Data_Analysis/ComparingLCAvCFA",
#    recursive=TRUE,
#    showOutput=TRUE)
#  

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  
#  runModels_Interactive()
#  

## ----echo = FALSE, out.width = "80%", fig.pos="h", fig.cap = "Figure. Example of using runModels() in an interactive graphical interface."----

knitr::include_graphics("runModels_Interactive-Screenshot.png")


## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  
#  allOutput <- readModels(
#    "C:/Data_Files/CFANesting",
#    recursive=TRUE)
#  
#  ## assuming there are multiple files in this directory
#  ## just model summaries could retained as a data.frame as follows:
#  
#  library(plyr)
#  justSummaries <- do.call("rbind.fill",
#    sapply(allOutput,"[", "summaries"))
#  

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  
#  mySummaries <- extractModelSummaries(
#    "C:/Data_Analysis/ComparingLCAvCFA",
#    recursive = TRUE)
#  

## ----eval=FALSE, echo=FALSE---------------------------------------------------
#  
#  summaryStats <- extractModelSummaries(
#    "C:/Program Files/Mplus/Mplus Examples/User's Guide Examples/Outputs",
#    filefilter="ex4.*")
#  

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  
#  summaryStats <- extractModelSummaries(
#    "C:/Data_Analysis/Multiclass Models",
#    filefilter="[123]{1}-class.*Threshold.*")
#  

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  
#  showSummaryTable(
#    summaryStats,
#    keepCols = c("Title", "LL", "AIC", "BIC", "CFI"),
#    sortBy = "AIC")
#  

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  
#  showSummaryTable(
#    summaryStats,
#    dropCols = c("InputInstructions", "Observations", "Parameters"),
#    sortBy = "CFI")
#  

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  
#  HTMLSummaryTable(
#    summaryStats,
#    filename = "C:/MyModelSummary.html",
#    display = TRUE,
#    keepCols = c("Title", "LL", "AIC", "BIC", "AICC"),
#    sortBy = "AIC")
#  

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  
#  myLatexTable <- LatexSummaryTable(
#    summaryStats,
#    keepCols = c("Title", "BIC", "Parameters"),
#    sortBy = "Parameters",
#    caption = "Comparing CFA vs. LCA according to number of parameters",
#    label="CFALCATab")
#  

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  
#  modelResults <- extractModelParameters(
#    "C:/Data_Analysis/Mplus Output.out")
#  

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  
#  unstandardizedResults <- modelResults$unstandardized
#  
#  #equivalently
#  standardizedResults <- modelResults[["stdyx.standardized"]]
#  

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  
#  allModelParameters <- extractModelParameters(
#    "C:/Data_Analysis/ComparingLCAvCFA",
#    recursive = TRUE)
#  

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  names(allModelParameters)
#  
#  ## ComparingLCAvCFA.LCA.1.class.LCA.out
#  ## ComparingLCAvCFA.LCA.2.class.LCA.out
#  ## ComparingLCAvCFA.LCA.3.class.LCA.out
#  ## ComparingLCAvCFA.CFA.1.factor.CFA.out
#  ## ComparingLCAvCFA.CFA.2.factor.CFA.out
#  ## ComparingLCAvCFA.CFA.3.factor.CFA.out
#  

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  
#  TwoFacCFA.STDYX <- allModelParameters$ComparingLCAvCFA.CFA.2.factor.CFA.out$stdyx.standardized
#  

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  
#  unstandardizedOnly <- sapply(allModelParameters, "[", "unstandardized")
#  

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  
#  oldNames <- names(allModelParameters)
#  unstandardizedOnly <- sapply(allModelParameters, "[", "unstandardized")
#  names(unstandardizedOnly) <- oldNames
#  

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  
#  #add the filename as a field in the data.frame (so it's uniquely identified when combined)
#  lapply(names(unstandardizedOnly), function(element) {
#  	unstandardizedOnly[[element]]$filename <<- element
#  })
#  
#  #this will only work if all data.frames have identical columns (i.e., same Mplus output fields)
#  combinedParameters <- do.call("rbind", unstandardizedOnly)
#  

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  
#    library(MplusAutomation)
#    library(ggplot2)
#    modelParams <- extractModelParameters("output_to_plot.out)$unstandardized
#    modelParams <- subset(modelParams,
#      paramHeader=="Means" &
#        LatentClass != "Categorical.Latent.Variables",
#      select=c("LatentClass", "param", "est", "se"))
#  
#    limits <- aes(ymax = est + se, ymin=est - se)
#  	
#  	fmmMeanPlot <- ggplot(modelParams, aes(x=param, y=est)) +
#  	    geom_pointrange(limits) +
#  	    scale_x_discrete("") +
#  	    geom_hline(yintercept=0, color="grey50") +
#  	    facet_grid(LatentClass ~ .) +
#  	    theme_bw() +
#  	    ylab("Mean Value") +
#  	    coord_flip()
#  	print(fmmMeanPlot)
#  

## ----echo = FALSE, out.width = "80%", fig.pos="h", fig.cap = "Figure. Example of graphing finite mixutre model results from Mplus using ggplot2."----

knitr::include_graphics("mplusAutomationFMMPlot.png")


## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  
#  parallelModels <- readModels("10_14_Harsh_SelfCon_Impul")
#  
#  compareModels(parallelModels[["backport.from.grand.model.out"]],
#    parallelModels[["backport.from.grand.model.slopesonw1.out"]],
#    show = c("diff", "pdiff", "summaries", "unique"),
#    equalityMargin = c(param = .05, pvalue = .02),
#    sort = "type", diffTest = TRUE, showNS = FALSE)
#  

