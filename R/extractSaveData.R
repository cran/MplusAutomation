#PARSE SAVEDATA INFORMATION FROM MPLUS OUTPUT FILE
getSavedata_Fileinfo <- function(outfile) {
  #wraps l_getSavedata_Fileinfo by checking for the outfile and reading it as a character vector.
  
  #note that outfile is assumed to be a full path to the file (or to be in the current working directory)
  #helper function to parse output from savedata command
  #if returnData is true, the data file created will be read in as an R data frame
  #if returnData is false, just the variable names are returned
  #considering using addHeader to prepend a header row
  
  if(!file.exists(outfile)) {
    stop("Cannot locate outfile: ", outfile)
  }
  
  outfiletext <- scan(outfile, what="character", sep="\n", strip.white=FALSE, blank.lines.skip=FALSE)
  
  if (length(outfiletext) == 0) {
    warning("Empty outfile")
    return(NULL)
  }
  
  return(l_getSavedata_Fileinfo(outfile, outfiletext))
  
}

#local function that does the work of getSaveData_Fileinfo
#split out so that getSaveData_Fileinfo is exposed to user, but parsing function can be used by readModels
l_getSavedata_Fileinfo <- function(outfile, outfiletext) {
 
  require(gsubfn)
  require(plyr)
  
  sectionStarts <- c("Order and format of variables", #output from FILE= command (analysis output)
      "Estimated Covariance Matrix for the Parameter Estimates", #tech3
      "Estimated Means and Covariance Matrix for the Latent Variables", #tech4
      "Sample/H1/Pooled-Within Matrix", #sample
      "Bayesian Parameters", #bparameters
      "Within and between sample statistics with Weight matrix", #swmatrix
      "Estimates", #estimates
	  "Order of variables" #monte carlo
  )
  
  #extract entire savedata section
  savedataSection <- getSection("^\\s*SAVEDATA INFORMATION\\s*$", outfiletext)
  
  #need to have SAVEDATA section to proceed
  if (is.null(savedataSection)) {
    #omit warning -- probably more common for section to be missing
    #warning("Unable to locate a SAVEDATA section in output file: ", outfile)
    return(NULL)
  }
  
  #initialize these variables to empty character strings so that list return value is complete
  #important in cases where some savedata output available, but other sections unused
  listVars <- c("fileName", "fileVarNames", "fileVarFormats", "fileVarWidths", "bayesFile", "bayesVarNames")
  l_ply(listVars, assign, value=NA_character_, envir=environment())
  
  #process FILE= section (individual-level data from analysis)
  fileSection <- getSection("^\\s*Order and format of variables\\s*$", savedataSection, sectionStarts)
  
#  savedataSection <- outfiletext[savedataStart:length(outfiletext)]
#  
#  fileSectionStart <- grep("^\\s*Order and format of variables\\s*$", savedataSection, ignore.case=TRUE, perl=TRUE)
#  fileSectionEnd <- grep("^\\s*Save file record length\\s+\\d+$", savedataSection, ignore.case=TRUE, perl=TRUE)
  
#  if (length(fileSectionStart) > 0 && length(fileSectionEnd) > 0) {
#    fileSection <- savedataSection[fileSectionStart:fileSectionEnd]
  
  if (!is.null(fileSection)) {
    #save data section exists, but doesn't contain this output. Maybe other savedata stuff, like bayesian, tech4, etc.
    saveFileStart <- grep("^\\s*Save file\\s*$", fileSection, ignore.case=TRUE, perl=TRUE)
       
    #dump any blank fields because they will cause nulls in the names, formats, widths.
    #This is handled by blank.lines.skip=TRUE in wrappers, but readModels needs to retain blank lines
    #for other functions, so strip here.
    variablesToParse <- fileSection[1:(saveFileStart-1)]
    variablesToParse <- variablesToParse[variablesToParse != ""]
    
    fileVarNames <- sub("^\\s*([\\w\\d\\.]+)\\s+[\\w\\d\\.]+\\s*$", "\\1", variablesToParse, perl=TRUE)
    fileVarFormats <- sub("^\\s*[\\w\\d\\.]+\\s+([\\w\\d\\.]+)\\s*$", "\\1", variablesToParse, perl=TRUE)
    fileVarWidths <- strapply(fileVarFormats, "[IEFG]+(\\d+)(\\.\\d+)*", as.numeric, perl=TRUE, simplify=TRUE)
    
    #trim leading and trailing space from the filename
    fileName <- trimSpace(fileSection[saveFileStart+1])
    
  }
  
  #Monte carlo output
  mcSection <- getSection("^\\s*Order of variables\\s*$", savedataSection, sectionStarts)
  
  if (!is.null(mcSection)) {
	  #save data section exists, but doesn't contain this output. Maybe other savedata stuff, like bayesian, tech4, etc.
	  saveFileStart <- grep("^\\s*Save file\\s*$", mcSection, ignore.case=TRUE, perl=TRUE)
	  
	  #dump any blank fields because they will cause nulls in the names, formats, widths.
	  #This is handled by blank.lines.skip=TRUE in wrappers, but readModels needs to retain blank lines
	  #for other functions, so strip here.
	  variablesToParse <- mcSection[1:(saveFileStart-1)]
	  variablesToParse <- variablesToParse[variablesToParse != ""]
	  
	  #just have variable names, no formats
    fileVarNames <- trimSpace(variablesToParse)
	  
	  #trim leading and trailing space from the filename
	  fileName <- trimSpace(mcSection[saveFileStart+1])
	  
  }
  
  #Bayesian parameters section
  bparametersSection <- getSection("^\\s*Bayesian Parameters\\s*$", savedataSection, sectionStarts)
    
  if (!is.null(bparametersSection)) {
    
    bayesFileStart <- grep("^\\s*Save file\\s*$", bparametersSection, ignore.case=TRUE, perl=TRUE)
    
    if (length(bayesFileStart > 0)) {
      
      bayesFile <- trimSpace(bparametersSection[bayesFileStart+1])
      paramOrderStart <- grep("^\\s*Order of parameters saved\\s*$", bparametersSection, ignore.case=TRUE, perl=TRUE)
      paramOrderSection <- sapply(bparametersSection[(paramOrderStart+2):length(bparametersSection)], trimSpace, USE.NAMES=FALSE) #trim leading/trailing spaces and skip "Order of parameters" line and the subsequent blank line

      #parameters list ends with a blank line, so truncate section at next newline
      #or in case where section ends after last param, then no blank line is present (just end of vector), so do nothing
      nextBlankLine <- which(paramOrderSection == "")
      if (length(nextBlankLine) > 0) paramOrderSection <- paramOrderSection[1:(nextBlankLine[1]-1)]
     
      #rather than split into two columns, concatenate so that these are workable as column names
      #paramOrderSection <- strsplit(paramOrderSection, "\\s*,\\s*", perl=TRUE)
      #bayesVarTypes <- gsub("\\s+", "\\.", sapply(paramOrderSection, "[", 1), perl=TRUE)
      #bayesVarNames <- gsub("\\s+", "\\.", sapply(paramOrderSection, "[", 2), perl=TRUE)
    
      #15Mar2012: Workaround for bug: means for categorical latent variables are in output file listing
      #but these are not actually present in bparameters. Filter out
      paramOrderSection <- paramOrderSection[!grepl("Parameter\\s+\\d+, \\[ [^#]#\\d+ \\]", paramOrderSection, perl=TRUE)]
       
      bayesVarNames <- gsub("\\s*,\\s*", "_", paramOrderSection, perl=TRUE)
      bayesVarNames <- gsub("\\[", "MEAN", bayesVarNames, perl=TRUE)
      bayesVarNames <- gsub("\\s*\\]\\s*", "", bayesVarNames, perl=TRUE)
      bayesVarNames <- gsub("\\s+", ".", bayesVarNames, perl=TRUE)
      
    }    
  } 
  
  #future: plausible values output from Bayesian runs
  #PLAUSIBLE VALUE MEAN, MEDIAN, SD, AND PERCENTILES FOR EACH OBSERVATION
  
  #return the file information as a list
  return(list(fileName=fileName, fileVarNames=fileVarNames, fileVarFormats=fileVarFormats, fileVarWidths=fileVarWidths,
          bayesFile=bayesFile, bayesVarNames=bayesVarNames)) #bayesVarTypes=bayesVarTypes, 
}

#READ FILE= SAVEDATA OUTPUT
getSavedata_Data <- function(outfile) {
  #exposed wrapper for l_getSavedata_Data, which pulls saveData data into data.frame
  if(!file.exists(outfile)) {
    stop("Cannot locate outfile: ", outfile)
  }
  
  outfiletext <- scan(outfile, what="character", sep="\n", strip.white=FALSE, blank.lines.skip=FALSE)
  
  if (length(outfiletext) == 0) {
    warning("Empty outfile")
    return(NULL)
  }
  
  fileInfo <- l_getSavedata_Fileinfo(outfile, outfiletext)
  
  if (is.null(fileInfo) || all(is.na(fileInfo))) return(NULL)
  else if (is.na(fileInfo[["fileVarWidths"]][1])) return(l_getSavedata_readRawFile(outfile, outfiletext, format="free", fileName=fileInfo[["fileName"]], 
            varNames=fileInfo[["fileVarNames"]]))
  else return(l_getSavedata_readRawFile(outfile, outfiletext, format="fixed", fileName=fileInfo[["fileName"]], 
            varNames=fileInfo[["fileVarNames"]], varWidths=fileInfo[["fileVarWidths"]]))
}

getSavedata_Bparams <- function(outfile) {
  #exposed wrapper for l_getSavedata_readRawFile, which pulls bayesian parameters into a data.frame
  if(!file.exists(outfile)) {
    stop("Cannot locate outfile: ", outfile)
  }
  
  outfiletext <- scan(outfile, what="character", sep="\n", strip.white=FALSE, blank.lines.skip=FALSE)
  
  if (length(outfiletext) == 0) {
    warning("Empty outfile")
    return(NULL)
  }
  
  fileInfo <- l_getSavedata_Fileinfo(outfile, outfiletext)
  
  if (is.null(fileInfo) || all(is.na(fileInfo))) return(NULL)
  else return(l_getSavedata_readRawFile(outfile, outfiletext, format="free", fileName=fileInfo[["bayesFile"]], varNames=fileInfo[["bayesVarNames"]]))
  
}

l_getSavedata_readRawFile <- function(outfile, outfiletext, format="fixed", fileName, varNames, varWidths) {
  #browser()
  outfileDirectory <- splitFilePath(outfile)$directory

  #if file requested is missing, then abort data pull
  if (is.null(fileName) || is.na(fileName) || is.null(varNames) || is.na(varNames)) return(NULL)
  
  savedataSplit <- splitFilePath(fileName)
  
  #if outfile target directory is non-empty, but savedataFile is without directory, then append
  #outfile directory to savedataFile. This ensures that R need not be in the working directory
  #to read the savedataFile. But if savedataFile has an absolute directory, don't append
  
  #if savedata directory is present and absolute, or if no directory in outfile, just use filename as is
  if (!is.na(savedataSplit$directory) && savedataSplit$absolute)
    savedataFile <- fileName #just use savedata filename if has absolute path
  else if (is.na(outfileDirectory))
    savedataFile <- fileName #just use savedata filename if outfile is missing path (working dir)
  else
    savedataFile <- file.path(outfileDirectory, fileName) #savedata path relative or absent and outfile dir is present
  
  #cat("Outfile dir: ", outfileDirectory, "\n")
  #cat("Savedata directory: ", savedataSplit$directory, "\n")
  #cat("concat result: ", savedataFile, "\n")
  
  #need to read as fixed width format given the way Mplus left-aligns missing vals (*)
  #dataset <- read.table(file=file.path(path, fileInfo$fileName), header=FALSE, 
  #    na.strings="*", col.names=fileInfo$varNames)
  
  if (format == "free") {
    #handle case where filename contains * indicating Monte Carlo/MI dataset with reps
    if (grepl("\\*", savedataSplit$filename, perl=TRUE)) {
      #resplit now that outfileDir and savedataDir are assembled
      resplit <- splitFilePath(savedataFile)
      
      #patch file pattern to match perl syntax
      pat <- gsub("\\.", "\\\\.", resplit$filename, perl=TRUE)
      pat <- gsub("\\*", "\\\\d+", pat, perl=TRUE)
      fileNames <- list.files(path=resplit$directory, pattern=pat, full.names=FALSE) #very klunky way to handle this
      fileList <- list.files(path=resplit$directory, pattern=pat, full.names=TRUE)
      
      dataset <- list()
      
      for (f in 1:length(fileList)) {
        dataset[[make.names(fileNames[f])]] <- read.table(file=fileList[f], header=FALSE,
            na.strings="*", col.names=varNames, strip.white=TRUE)
      }
    }
    else  
      dataset <- read.table(file=savedataFile, header=FALSE, na.strings="*", col.names=varNames, strip.white=TRUE)    
  }
  else if (format == "fixed") {
    if (!length(varWidths) > 0) stop("Fixed width file specified, but no widths obtained.")
    #strip.white is necessary for na.strings to work effectively with fixed width fields
    #otherwise would need something like "*       " for na.strings
  
    dataset <- read.fwf(file=savedataFile, widths=varWidths, header=FALSE,
        na.strings="*", col.names=varNames, strip.white=TRUE)
  }
  
  return(dataset)

}
