extractValue <- function(name, textToScan, filename, type="int") {
#extractValue(name, outfile, type="int")
#
#   name: the exact text to be matched in the outfile that identifies the parameter of interest
#   textToScan: the chunk of Mplus output to be parsed, passed as a vector of character strings (from the scan command).
#		filename: the name of the file containing textToScan. Used to make more intelligible warning messages.
#   type: the data type of the parameter, which determines the regexp used. Currently can be "int", "dec", "str", or "calc".
#
#   Description: An internal function used by modelParams to extract parameters from the output file using regular expressions.
#
  
  #locate the matching line in the output file
  matchlines <- grep(name, textToScan, ignore.case=TRUE, value=TRUE)
  
  #using as.numeric for NAs to avoid mixed data types, which mess up the xtable creation
  
  #calc fields calculated based on values extracted, so they aren't directly in the output
  if (type == "calc") {
    return(as.numeric(NA))
  }
  
  if (length(matchlines) > 1) {
    warning(paste("More than one match found for parameter: ", name, "\n  ", filename, sep=""))
    return(matchlines)
  }
  else if (length(matchlines) == 0) {
    #if the parameter of interest not found in this file, then return NA
    #warning(paste("Parameter not found: ", name, "\n  ", filename, sep=""))
    return(as.numeric(NA))
  }
  
  if (type == "int") {
    regexp <- "-*\\d+" #optional negative sign in front
  }
  else if (type == "dec") {
    #regexpr: -*\\d+\\.\\d+ : -* optional negative sign, \\d+ match at least one digit \\. match decimal sign \\d+ match decimal digits
    regexp <- "-*\\d+\\.\\d+"
  }
  else if (type == "str") {
    regexp <- paste(name, ".*", sep="")
  }
  
  #locate the match
  start <- regexpr(regexp, matchlines[1], perl=TRUE)
  
  if (type == "str") {
    #remove the tag portion of the string (e.g., "title:"), retaining rest of line
    returnVal <- as.character(sub(name, "", matchlines[1], ignore.case=TRUE))    
  }
  #pull from the start of the match through match.length, which is the length of characters that matched
  #need to subtract one from the start + length offset to grab the correct number of characters
  #(e.g., if the match runs from 40-44, the start will be 40, with length 5, but 40 + 5 would be 6 characters, hence -1 
  else returnVal <- as.numeric(substr(matchlines[1], start, start + attr(start, "match.length") - 1))
  
  return(returnVal)
}

getMultilineSection <- function(header, outfile, filename) {
  headerRow <- grep(header, outfile, perl=TRUE)
  
  if (length(headerRow) == 1) {
    sectionStart <- headerRow + 1 #skip header row
    if (outfile[sectionStart] == "") sectionStart <- sectionStart + 1 #As far as I know, there is always a blank line after the header, so skip past it
    
    #Sections end with the next blank line in the file
    blankLines <- which(outfile=="")
    sectionEnd <- blankLines[blankLines > sectionStart][1] - 1 #subtract 1 to go to line preceding blank
    
    sectionText <- outfile[sectionStart:sectionEnd]
  }
  else {
    sectionText <- NA_character_
    if (length(headerRow) > 1) warning(paste("Multiple matches for header: ", header, "\n  ", filename, sep=""))
    #else if (length(headerRow) < 1) warning(paste("Could not locate section based on header: ", header, "\n  ", filename, sep=""))
  }
  
  return(sectionText)
  
}

#Moving forward, specify model fit statistics as nested within sections. Develop general syntax along the lines of:
#"AIC{dec},BIC{dec},AICC{dec}|Information Criteria"
#This allows for all fit criteria to be nested within sections and to avoid the junky syntax below
#But in the extract section, would not be very user-friendly


modelParams <- function(outfile, filename, extract=c("Title", "LL", "BIC", "AIC", "AICC",
  "Parameters", "Observations", "BLRT", "RMSEA", "CFI", "TLI", "ChiSqModel", "aBIC", 
  "Estimator", "SRMR", "WRMR", "ChiSqBaseline"))
{
#modelParams(outfile, filename)
#   outfile: this is the output file in string form to be parsed. Passed in from extractModelSummaries.
#   filename: name of the file being parsed. Used in case of bad model, prints a warning.
#
#   Description: This function parses an output file for specific model details.
#   It returns a list of model details for a single output file.
  
  #automatically extract the input for the file.
  startInput <- grep("^\\s*INPUT INSTRUCTIONS\\s*$", outfile, ignore.case=TRUE, perl=TRUE)
  if (length(startInput) == 0) warning("Could not find beginning of input")
     
  endInput <- grep("^\\s*(INPUT READING TERMINATED NORMALLY|\\d+ WARNING\\(S\\) FOUND IN THE INPUT INSTRUCTIONS)\\s*$", outfile, ignore.case=TRUE, perl=TRUE)
  if (length(endInput) == 0) warning("Could not find end of input")
  
  #check for BLRT, set it to extract, but remove it as a field per se
  #the BLRT keyword serves as a placeholder for extracting many fields, like km1likelihood
  processBLRT <- "BLRT" %in% extract
  if (processBLRT) {
    #delete it, which prevents subsequent attempts to search for BLRT
    extract <- extract[!extract=="BLRT"]
  }
  
  #all of these "exceptions" to the extract loop are seeming more and more kludgy
  #maybe the default shouldn't be to run extractValue on all extract fields
  
  processRMSEA <- "RMSEA" %in% extract
  if (processRMSEA) extract <- extract[!extract=="RMSEA"]
  
  #title processing is funky because it can span multiple lines
  processTitle <- "Title" %in% extract
  if (processTitle) extract <- extract[!extract=="Title"]
  
  #CFI/TLI are in their own section
  processCFI <- "CFI" %in% extract
  if (processCFI) extract <- extract[!extract=="CFI"]
  
  processTLI <- "TLI" %in% extract
  if (processTLI) extract <- extract[!extract=="TLI"]
  
  processChiSqModel <- "ChiSqModel" %in% extract
  if (processChiSqModel) extract <- extract[!extract=="ChiSqModel"]
  
  processChiSqBaseline <- "ChiSqBaseline" %in% extract
  if (processChiSqBaseline) extract <- extract[!extract=="ChiSqBaseline"]
    
  processEstimator <- "Estimator" %in% extract
  if (processEstimator) extract <- extract[!extract=="Estimator"]
  
  processSRMR <- "SRMR" %in% extract
  if (processSRMR) extract <- extract[!extract=="SRMR"]
  
  processWRMR <- "WRMR" %in% extract
  if (processWRMR) extract <- extract[!extract=="WRMR"]
  
  
  expandField <- function(name) {
    #internal function used to convert short keyword names for parameters into their full Mplus output equivalents
    #returns a vector of the exact string and the data type ("calc", "dec", "int", or "str").
    field <- switch(EXPR=name,
        Title = c("title: ", "str"),
        LL = c("H0 Value", "dec"),
        BIC = c("Bayesian \\(BIC\\)", "dec"),
        AIC = c("Akaike \\(AIC\\)", "dec"),
        AICC = c("AICC", "calc"),
        Parameters = c("Number of Free Parameters", "int"),
        aBIC = c("Sample-Size Adjusted BIC", "dec"),
        Entropy = c("Entropy", "dec"),
        Observations = c("Number of observations", "int"),
        CFI= c("CFI", "dec"),
        TLI = c("TLI", "dec"),
        Estimator = c("estimator", "str"),
        c(name, "calc") #return untouched if did not match
    )
    
    return(field)
  }
  
  #convert keyword names into full strings for matching
  extractDetailed <- sapply(extract, expandField, USE.NAMES=FALSE)
  
  #preallocates list
  arglist = vector("list", length(extract))  
  
  #iterate over each parameter to extract
  for (i in 1:length(extract)) {
    thisName <- extractDetailed[1,i]
    thisType <- extractDetailed[2,i]    
    arglist[i] <- extractValue(name=thisName, outfile, filename, type=thisType)
  }
  
  #use the short keywords as the variable names	
  names(arglist) <- extract

	#Only warn about missing LL for ML-based estimators
  if ("LL" %in% extract && !is.na(arglist$Estimator) && arglist$Estimator %in% c("ML", "MLR", "MLM", "MLMV", "MLF") && is.na(arglist$LL)) {
		warning("Model missing LL value, despite use of ML-based estimator. Likely a failed run.\n  ", filename)
  }
  
  #handle AICC calculation, requires AIC, Parameters, and observations
  if (all(c("AICC", "AIC", "Parameters", "Observations") %in% extract)) {
    #calculate adjusted AIC per Burnham & Anderson(2004), which is better than AIC for non-nested model selection
    arglist$AICC <- arglist$AIC + (2*arglist$Parameters*(arglist$Parameters+1))/(arglist$Observations-arglist$Parameters-1)  
  }
  
  if (processBLRT) {
    
    #locate the beginning of the BLRT section
    matchlines <- grep("TECHNICAL 14 OUTPUT", outfile)
    
    if (length(matchlines) == 1) {
      #match up through the end of the file
      endRange <- grep("3463 Stoner Ave\\.", outfile)
      
      if (!length(endRange) == 1) {
        stop("Problem identifying end marker for BLRT")
      }
      
      blrtpiece <- outfile[matchlines:endRange]
      
      arglist$BLRT_KM1LL <- extractValue(name="H0 Loglikelihood Value", blrtpiece, filename, type="dec")
      arglist$BLRT_PValue <- extractValue(name="Approximate P-Value", blrtpiece, filename, type="dec")
      arglist$BLRT_Numdraws <- extractValue(name="Successful Bootstrap Draws", blrtpiece, filename, type="int")
    }
    else {
      #warning("Could not locate BLRT section, despite being requested")
      
      #need to pad the expected fields with NAs to keep the list length consistent, permitting correct rbind
      arglist$BLRT_KM1LL <- as.numeric(NA)
      arglist$BLRT_PValue <- as.numeric(NA)
      arglist$BLRT_Numdraws <- as.numeric(NA)
    }
  }

  if (processChiSqModel) {
    ChiSqSection <- getMultilineSection("^\\s*Chi-Square Test of Model Fit\\s*$", outfile, filename)
    arglist$ChiSqM_Value <- NA_real_
    arglist$ChiSqM_DF <- NA_integer_
    arglist$ChiSqM_PValue <- NA_real_

    if (!is.na(ChiSqSection[1])) {
      #need to use beginning of line caret to ensure that Value does not also match P-Value
      arglist$ChiSqM_Value <- extractValue(name="^\\s*Value", ChiSqSection, filename, type="dec")
      arglist$ChiSqM_DF <- extractValue(name="Degrees of Freedom", ChiSqSection, filename, type="int")
      arglist$ChiSqM_PValue <- extractValue(name="^\\s*P-Value", ChiSqSection, filename, type="dec")
    }
  }

  if (processChiSqBaseline) {
    ChiSqSection <- getMultilineSection("^\\s*Chi-Square Test of Model Fit for the Baseline Model\\s*$", outfile, filename)
    arglist$ChiSqBaseline_Value <- NA_real_
    arglist$ChiSqBaseline_DF <- NA_integer_
    arglist$ChiSqBaseline_PValue <- NA_real_
    
    if (!is.na(ChiSqSection[1])) {
      #need to use beginning of line caret to ensure that Value does not also match P-Value
      arglist$ChiSqBaseline_Value <- extractValue(name="^\\s*Value", ChiSqSection, filename, type="dec")
      arglist$ChiSqBaseline_DF <- extractValue(name="Degrees of Freedom", ChiSqSection, filename, type="int")
      arglist$ChiSqBaseline_PValue <- extractValue(name="^\\s*P-Value", ChiSqSection, filename, type="dec")
    }
  }
    
  if (processCFI || processTLI) {
    #default to missing in case section not present
    arglist$CFI <- NA_real_
    arglist$TLI <- NA_real_
    
    CFITLIsection <- getMultilineSection("^\\s*CFI/TLI", outfile, filename)
    
    if (!is.na(CFITLIsection[1])) {
      if (processCFI) arglist$CFI <- extractValue(name="CFI", CFITLIsection, filename, type="dec")
      if (processTLI) arglist$TLI <- extractValue(name="TLI", CFITLIsection, filename, type="dec")
    }
  }
  
  if (processRMSEA) {
    arglist$RMSEA_Estimate <- NA_real_
    arglist$RMSEA_90CI_LB <- NA_real_
    arglist$RMSEA_90CI_UB <- NA_real_
    arglist$RMSEA_pLT05 <- NA_real_
    
    RMSEAsection <- getMultilineSection("RMSEA \\(Root Mean Square Error Of Approximation\\)", outfile, filename)

    if (!is.na(RMSEAsection[1])) {
      arglist$RMSEA_Estimate <- extractValue(name="Estimate", RMSEAsection, filename, type="dec")
      arglist$RMSEA_pLT05 <- extractValue(name="Probability RMSEA <= \\.05", RMSEAsection, filename, type="dec")
      
      CILine <- grep("90 Percent C.I.", RMSEAsection, fixed=TRUE)      
      if (length(CILine) > 0) {
        
        #form a vector of the lower and upper bounds
        CIBounds <- strapply(RMSEAsection[CILine], "^\\s*90 Percent C\\.I\\.\\s*([-\\d\\.]+)\\s+([-\\d\\.]+)\\s*$", c, perl=TRUE)[[1]]
        if (length(CIBounds) == 2) {
          arglist$RMSEA_90CI_LB <- as.numeric(CIBounds[1])
          arglist$RMSEA_90CI_UB <- as.numeric(CIBounds[2])          
        }
      }      
    }
  }
  
  if (processTitle) {
    titleStart <- grep("^\\s*title:", outfile, ignore.case=TRUE, perl=TRUE)
    if (length(titleStart) == 1) {
      keywords <- grep("^\\s*(data:|variable:|define:|analysis:|model:|output:|savedata:|plot:|montecarlo:)", outfile, ignore.case=TRUE, perl=TRUE)
      titleEnd <- keywords[keywords > titleStart][1] - 1 #subtract 1 to go to line preceding next keyword
      stopifnot(titleEnd > 0)
      
      #if title spans multiple lines, then collapse into one string
      title <- paste(outfile[titleStart:titleEnd], collapse=" ")
      
      #delete the "Title: " piece from the match       
      title <- sub("title:\\s+", "", title, ignore.case=TRUE)
      
      arglist$Title <- title
    }
    else {
      warning("Unable to locate title field. Returning missing")
      arglist$Title <- NA_character_
    }
  }
  
  if (processEstimator) {
    #the estimator is often defined in the input instructions, but we want the summary of the analysis section
    summaryStart <- grep("^\\s*SUMMARY OF ANALYSIS", outfile, ignore.case=TRUE, perl=TRUE)
    if (length(summaryStart) == 1) {
      bottomPortion <- outfile[summaryStart:length(outfile)]
      arglist$Estimator <- extractValue(name="^\\s*Estimator\\s*", bottomPortion, filename, type="str")
    }
  }
  
  if (processSRMR) {
    arglist$SRMR <- NA_real_
    SRMRSection <- getMultilineSection("SRMR \\(Standardized Root Mean Square Residual\\)", outfile, filename)
    
    if (!is.na(SRMRSection[1])) {
      arglist$SRMR <- extractValue(name="Value", SRMRSection, filename, type="dec")
    }
  }
  
  if (processWRMR) {
    arglist$WRMR <- NA_real_
    WRMRSection <- getMultilineSection("WRMR \\(Weighted Root Mean Square Residual\\)", outfile, filename)
    
    if (!is.na(WRMRSection[1])) {
      arglist$WRMR <- extractValue(name="Value", WRMRSection, filename, type="dec")
    }
  }
  
  #for now, skip including input instructions in the returned data.frame. Makes the output too cluttered.
  #arglist$InputInstructions <- paste((outfile[(startInput+1):(endInput-1)]), collapse="\n")
  arglist$Filename <- filename
      
  return(as.data.frame(arglist, stringsAsFactors=FALSE))
}

#could this also be used by runModels to locate input files?
#seems like that function would do well to allow for directories and single files, too.

getOutFileList <- function(target, recursive=FALSE, filefilter) {
	#This is a helper function used by extractModelSummaries and extractModelParameters.
	#It determines whether the target is a single file or a directory.
	#If it is a directory, all .out files are returned (perhaps recursively)
	#It also permits the files to be filtered using a certain regular expression.
	
	#determine whether target is a file or a directory
	if (file.exists(target)) {
		if (file.info(target)$isdir == TRUE) {
			
			#obtain list of all files in the specified directory
			filelist <- list.files(path=target, recursive=recursive, full.names=TRUE)
			
			#retain only .out files
			outfiles <- filelist[grep(".*\\.out$", filelist, ignore.case=TRUE)]
			
			if (!missing(filefilter)) {
				dropOutExtensions <- sapply(outfiles, function(x) {
							if (nchar(x) >= 4) return(tolower(substr(x, 1, (nchar(x)-4))))
						})
				outfiles <- outfiles[grep(paste(".*", filefilter, ".*", sep=""), dropOutExtensions, ignore.case=TRUE, perl=TRUE)]
			}      
		}
		else {
			#ensure that target is a single output file.
			if (nchar(target) >= 4 && !substr(target, nchar(target) - 3, nchar(target)) == ".out") stop("Specified target is not an output file.\n  Target:", target)
			
			#outfiles collection is just one file
			outfiles <- target      
		}
	}
	else stop("Specified target does not exist.\n  Target: ", target)
	
	if (length(outfiles) == 0) { 
		warning("No output files detected in this directory.")
		return(NULL)
	}
	
	return(outfiles)
}

extractModelSummaries <- function(target=getwd(), recursive=FALSE, filefilter) {
#extractModelSummaries(target, recursive=FALSE)
#
#   target: the directory containing Mplus output files to read. Use forward slashes in directory name (e.g., "C:/Users/Mplus/"). Defaults to working directory.
#   recursive: specifies whether to parse output files in subdirectories beneath the specified directory. Defaults to FALSE. (TRUE or FALSE)
#
#   Description: This function identifies all Mplus .out files in the specified directory (directory parameter)
#   and reads basic model fit information from each file. The function combines fit details across models into a list.
#
#   Example: myModels <- extractModelSummaries("C:/Documents and Settings/Michael/My Documents/Mplus Stuff/", recursive=TRUE)
  
  #retain working directory and reset at end of run

  curdir <- getwd()
  
	outfiles <- getOutFileList(target, recursive, filefilter)
	
  details <- c()
  
  #for each output file, use the modelParams function to extract relevant data
  #note that modelParams returns data as a list
  #rbind creates an array of lists by appending each modelParams return value
  for (i in 1:length(outfiles)) {
    #read the file
    readfile <- scan(outfiles[i], what="character", sep="\n", strip.white=TRUE, blank.lines.skip=FALSE)
    
    #bomb out for EFA files
    if (length(grep("TYPE\\s+(IS|=|ARE)\\s+((MIXTURE|TWOLEVEL)\\s+)*EFA\\s+\\d+", readfile, ignore.case=TRUE, perl=TRUE)) > 0) {
      warning(paste("EFA, MIXTURE EFA, and TWOLEVEL EFA files are not currently supported by extractModelSummaries.\n  Skipping outfile: ", outfiles[i], sep=""))
      next #skip file
    }
    
    #append params for this file to the details array
    #note that this is a memory-inefficient solution because of repeated copying. Better to pre-allocate.
    details <- rbind(details, modelParams(readfile, outfiles[i]))
  }
  
  #reset working directory
  setwd(curdir)
  
  return(details)
}

getSavedata_Data <- function(outfile) {
  #outfile should be a relative or absolute path to the .out file with the savedata section to be parsed
  #if no directory is provided, the file is assumed with be within the working directory getwd().
  
  #extract the path (the data file itself should be located in the same spot as the outfile 
  components <- strsplit(outfile, split="[\\/]")[[1]]
  lcom <- length(components)
  
  #if there is at least one directory separator in the mix (i.e., not just a filename), then
  #delete the last element in the array, which is the filename, thereby retaining only the path
  if (lcom > 1) components <- components[-lcom]
  
  #construct the file path from the elements of the array
  path <- do.call("file.path", as.list(components))
  
  fileInfo <- getSavedata_Fileinfo(outfile)
  
  #need to read as fixed width format given the way Mplus left-aligns missing vals (*)
  #dataset <- read.table(file=file.path(path, fileInfo$fileName), header=FALSE, 
  #    na.strings="*", col.names=fileInfo$varNames)
  
  #strip.white is necessary for na.strings to work effectively with fixed width fields
  #otherwise would need something like "*       " for na.strings
  dataset <- read.fwf(file=file.path(path, fileInfo$fileName), widths=fileInfo$varWidths, header=FALSE,
      na.strings="*", col.names=fileInfo$varNames, strip.white=TRUE)
  
  return(dataset)
}

addHeaderToSavedata <- function(outfile, directory=getwd()) {
  
}

getSavedata_Fileinfo <- function(outfile) {
  #note that outfile is assumed to be a full path to the file (or to be in the current working directory)
  #helper function to parse output from savedata command
  #if returnData is true, the data file created will be read in as an R data frame
  #if returnData is false, just the variable names are returned
  #considering using addHeader to prepend a header row
  
  require(gsubfn)
  
  if(!file.exists(outfile)) {
    stop("Cannot locate outfile: ", outfile)
  }
  
  readfile <- scan(outfile, what="character", sep="\n", strip.white=TRUE)
  
  if (length(readfile) == 0) {
    warning("Empty outfile")
    return(NULL)
  }
  
  savedataStart <- grep("^\\s*SAVEDATA INFORMATION\\s*$", readfile, ignore.case=TRUE, perl=TRUE)
  savedataEnd <- grep("^\\s*Save file record length\\s+\\d+$", readfile, ignore.case=TRUE, perl=TRUE)
  
  #need to have beginning and end
  stopifnot(length(savedataStart) > 0, length(savedataEnd) > 0)
  
  savedataSection <- readfile[savedataStart:savedataEnd]
  
  orderStart <- grep("^\\s*Order and format of variables\\s*$", savedataSection, ignore.case=TRUE, perl=TRUE)
  saveFileStart <- grep("^\\s*Save file\\s*$", savedataSection, ignore.case=TRUE, perl=TRUE)
  
  variablesToParse <- savedataSection[(orderStart+1):(saveFileStart-1)]
  
  variableNames <- sub("^([\\w\\d\\.]+)\\s+[\\w\\d\\.]+\\s*$", "\\1", variablesToParse, perl=TRUE)
  variableFormats <- sub("^[\\w\\d\\.]+\\s+([\\w\\d\\.]+)\\s*$", "\\1", variablesToParse, perl=TRUE)
  
  variableWidths <- strapply(variableFormats, "[IEFG]+(\\d+)(\\.\\d+)*", as.numeric, perl=TRUE, simplify=TRUE)
  
  #trim leading and trailing space from the filename
  fileName <- sub("^\\s*","", savedataSection[saveFileStart+1], perl=TRUE)
  fileName <- sub("\\s*$","", fileName, perl=TRUE)
  
  #return the file information as a list
  return(list(fileName=fileName, varNames=variableNames, varFormats=variableFormats, varWidths=variableWidths))
  
}

#a helper function to be used by wrappers that generate HTML, LaTex, and on-screen displays of summary statistics
subsetModelList <- function(modelList, keepCols, dropCols, sortBy) {
  #if did not pass either drop or keep, setup useful defaults
  if (missing(keepCols) && missing(dropCols)) keepCols <- c("Title", "LL", "Parameters", "AIC", "AICC", "BIC", "RMSEA_Estimate")
  
  if (missing(sortBy)) sortBy <- "AICC"
  
  #only allow keep OR drop.
  if(!missing(keepCols) && !missing(dropCols)) stop("keepCols and dropCols passed to subsetModelList. You must choose one or the other, but not both.")
  
  #keep only columns specified by keepCols
  if (!missing(keepCols) && length(keepCols) > 0) {
    MplusData <- modelList[, keepCols]
  }
  
  #drop columns specified by dropCols
  if (!missing(dropCols) && length(dropCols) > 0) {
    MplusData <- modelList
    #Process vector of columns to drop
    for (column in dropCols) {
      MplusData[[column]] <- NULL
    }
    
  }
  
  #make a list of non-missing columns
  notMissing <- unlist(lapply(names(MplusData), function(column) {
            if(!all(is.na(MplusData[[column]]))) return(column)
          }))
  
  #sort data set correctly and drop columns where all models are missing
  MplusData <- MplusData[order(MplusData[[sortBy]]), notMissing]
  
  return(MplusData)
}

#display summary table in a separate window
showSummaryTable <- function(modelList, keepCols, dropCols, sortBy) {
  require(relimp)

  MplusData <- subsetModelList(modelList, keepCols, dropCols, sortBy)
  showData(MplusData, font="Courier 9", placement="+30+30", maxwidth=150, maxheight=50, rownumbers=FALSE, title="Mplus Summary Table")
}

#create HTML table
HTMLSummaryTable <- function(modelList, filename=file.path(getwd(), "Model Comparison.html"), keepCols, dropCols, sortBy, display=FALSE) {
  require(xtable)
  #create HTML table and write to file.
  
  #ensure that the filename has a .html or .htm at the end
  if (!length(grep(".*\\.htm[l]*", filename)) > 0) {
    filename <- paste(filename, ".html", sep="")
  }                                      
  
  if (length(grep("[\\/]", filename)) == 0) {
    #Filename does not contain a path. Therefore, add the working directory
    filename <- file.path(getwd(), filename)
  }
  
  MplusData <- subsetModelList(modelList, keepCols, dropCols, sortBy)
  
  print(x=xtable(MplusData),
        type="html",
        file=filename,
        include.rownames = FALSE,
        NA.string = "."
  )
    
  if (display) {
    #load table in browser
    shell.exec(paste("file:///", filename, sep=""))
  }
 
}

LatexSummaryTable <- function(modelList, keepCols, dropCols, sortBy, label=NULL, caption=NULL) {
  #return latex table to caller
  require(xtable)
  
  MplusData <- subsetModelList(modelList, keepCols, dropCols, sortBy)

  return(xtable(MplusData, label=label, caption=caption))
}

#removed input instructions from routine extraction
#dropCols=c("InputInstructions", "Observations")

createTable <- function(modelList, filename=file.path(getwd(), "Model Comparison.html"),
  sortby="AICC", display=TRUE, latex=FALSE, dropCols=c("Observations"), label=NULL) {
#createTable(directory, recursive=FALSE)
#
#   modelList: list of model details returned by extractModelSummaries.
#   basedir: directory in which to save the HTML table. Defaults to current directory.
#   filename: name of HTML table file. Defaults to model comparison.html
#   sortby: name of field on which to sort. Defaults to "AICC". "BIC" and "AIC" are options.
#   display: whether to load the HTML table in the browser after creating it. Defaults to TRUE. (TRUE/FALSE) 
#
#   Description: This function generates an HTML table from a list of models generated by extractModelSummaries.
#
#   Example: createTable(myModels, "C:/Documents and Settings/Michael/My Documents/Mplus Stuff/", "my comparison.html", sortby="BIC")
  
  #retain working directory to reset at end of run
  #curdir <- getwd()
  #setwd(basedir)
  
  require(xtable)
  
  #convert modelList (which defaults to array of lists) to data frame
  dframe <- as.data.frame(modelList)
  
  #Process vector of columns to drop
  for (column in dropCols) {
    dframe[[column]] <- NULL
  }
  
  #sort the data frame according the sortby value
  sortTab <- dframe[order(unlist(dframe[, sortby])), ]
  
  #note that the sorting was previously not working because unlist automatically drops NULL values
  #switched code to use NAs, which is more appropriate.
  
  #ensure that the filename has a .html or .htm at the end
  if (!length(grep(".*\\.htm[l]*", filename)) > 0) {
    filename <- paste(filename, ".html", sep="")
  }                                      
  
  if (length(grep("[\\/]", filename)) == 0) {
    #Filename does not contain a path. Therefore, add the working directory
    filename <- file.path(getwd(), filename)
  }
  
  if (latex==FALSE) {
    print(
        x=xtable(sortTab),
        type="html",
        file=filename,
        include.rownames = FALSE,
        NA.string = "."
    )
    
    if (display) {
      #load table in browser
      shell.exec(paste("file:///", filename, sep=""))
    }
  }  
  
  #reset working directory
  #setwd(curdir)
  
  if (latex==TRUE) return(xtable(sortTab, label=label))
  
}

#Helper function for extractModelParameters. Used to parse each subsection of output within a given file and given results section (e.g., stdyx section) 
#There will be many chunks if latent classes, multiple groups, multilevel features are used.
extractParameters_1chunk <- function(thisChunk, columnNames) {
	if (missing(thisChunk) || is.na(thisChunk) || is.null(thisChunk)) stop("Missing chunk to parse.")
	if (missing(columnNames) || is.na(columnNames) || is.null(columnNames)) stop("Missing column names for chunk.")
	
	#okay to match beginning and end of line because strip.white used in scan
	matches <- gregexpr("^((Means|Thresholds|Intercepts|Variances|Residual Variances|New/Additional Parameters)|([\\w_\\d+\\.#]+\\s+(BY|WITH|ON|\\|)))$", thisChunk, perl=TRUE)
	
	#more readable (than above) using ldply from plyr
	convertMatches <- ldply(matches, function(row) data.frame(start=row, end=row+attr(row, "match.length")-1))
	
	#beware faulty logic below... assumes only one match per line (okay here)
	convertMatches$startline <- 1:nrow(convertMatches)
	
	#only keep lines with a single match
	#this removes rows that are -1 from gregexpr
	convertMatches <- subset(convertMatches, start > 0)
	
	#develop a dataframe that divides into keyword matches versus variable matches
	convertMatches <- ddply(convertMatches, "startline", function(row) {
				#pull the matching keyword based on the start/end attributes from gregexpr
				match <- substr(thisChunk[row$startline], row$start, row$end)
				
				#check for keyword
				if (match %in% c("Means", "Thresholds", "Intercepts", "Variances", "Residual Variances", "New/Additional Parameters")) {
					return(data.frame(startline=row$startline, keyword=make.names(match), varname=NA_character_, operator=NA_character_))
				}
				else if (length(variable <- strapply(match, "^\\s*([\\w_\\d+\\.#]+)\\s+(BY|WITH|ON|\\|)\\s*$", c, perl=TRUE)[[1]]) > 0) {
					return(data.frame(startline=row$startline, keyword=NA_character_, varname=variable[1], operator=variable[2]))
				}
				else stop("failure to match keyword: ", match)
			})
	
	comboFrame <- c()
	
	#convertMatches will now contain a data.frame marking the section headers for the chunk
	#example:
	#		startline keyword varname operator endline
	#		        7    <NA>      FW       BY      12
	#		       13    <NA>      FW       ON      16
	
	for (i in 1:nrow(convertMatches)) {
		#define the end line for this match as the start of next match - 1 
		if (i < nrow(convertMatches)) convertMatches[i,"endline"] <- convertMatches[i+1,"startline"]-1
		else convertMatches[i,"endline"] <- length(thisChunk) # or if last chunk in the section, just define as length
		
		#need +1 to eliminate header row from params 
		paramsToParse <- thisChunk[(convertMatches[i, "startline"]+1):convertMatches[i, "endline"]]
		
		#should result in a short list of params to parse (that belong to a given header i)
		#Example:
		#"U1                 0.557      0.036     15.470      0.000"
		#"U2                 0.638      0.038     16.751      0.000"
		#"U3                 0.660      0.038     17.473      0.000"
		#"U4                 0.656      0.037     17.585      0.000"
		
		#define the var title outside of the chunk processing because it will apply to all rows
		if (is.na(convertMatches[i,]$keyword)) varTitle <- paste(convertMatches[i,"varname"], ".", convertMatches[i,"operator"], sep="")
		else varTitle <- as.character(convertMatches[i,"keyword"])
		
		splitParams <- strsplit(paramsToParse, "\\s+", perl=TRUE)

		#rbind the split list as a data.frame 
		parsedParams <- data.frame(do.call("rbind", splitParams), stringsAsFactors=FALSE)
		
		#use the column names detected in extractParameters_1section
		names(parsedParams) <- columnNames
		
		#for each column, convert to numeric if it is. Otherwise, return as character
		parsedParams <- data.frame(lapply(parsedParams, function(col) {
							#a bit convoluted, but we want to test for a purely numeric string by using a regexp that only allows numbers, periods, and the minus sign
							#then sum the number of matches > 0 (i.e., where a number was found).
							#if the sum is the same as the length of the column, then all elements are purely numeric.

							if (sum(sapply(gregexpr("^[\\d\\.-]+$", col, perl=TRUE), "[", 1) > 0) == length(col)) return(as.numeric(col))
							else return(as.character(col))
						}), stringsAsFactors=FALSE)
		
		#add the paramHeader to the data.frame
		parsedParams$paramHeader <- varTitle

		#put the paramHeader at the front of the data.frame columns
		parsedParams <- parsedParams[,c("paramHeader", columnNames)]
		
		#add the current chunk to the overall data.frame
		comboFrame <- rbind(comboFrame, parsedParams)
		
	}
	
	#under the new strsplit strategy, just return the dataframe
	return(comboFrame)
	
}

extractParameters_1section <- function(modelSection, sectionName) {
	#extract model parameters for a given model results section. A section contains complete output for all parameters of a given type
	#(unstandardized, stdyx, stdy, or std) for a single file.
	#section name is used to name the list element of the returned list
	
	#helper function to detect model results columns
	detectColumnNames <- function(modelSection) {
		detectionFinished <- FALSE
		line <- 1
		while(detectionFinished == FALSE) {
			thisLine <- strsplit(modelSection[line], "\\s+", perl=TRUE)[[1]]
			if (line < length(modelSection)) nextLine <- strsplit(modelSection[line+1], "\\s+", perl=TRUE)[[1]]
			else nextLine <- NA_character_
			
			#detect common Mplus output formats
			#not especially flexible code, but hard to perfect it when names span two lines and headers have changed over versions
			#Would be ideal to build element-by-element, but not feasible given ambiguity across versions and two-line headers

			#Bayesian (ESTIMATOR=BAYES) 6-column output 
			if (identical(thisLine, c("Posterior", "One-Tailed", "95%", "C.I.")) &&
					identical (nextLine, c("Estimate", "S.D.", "P-Value", "Lower", "2.5%", "Upper", "2.5%")))
				varNames <- c("param", "est", "posterior_sd", "pval", "lower_2.5ci", "upper_2.5ci")
			
			#Usual five-column output that applies to most unstandardized and standardized sections in Mplus 5 and later
			else if (identical(thisLine, c("Two-Tailed")) && 
					identical(nextLine, c("Estimate", "S.E.", "Est./S.E.", "P-Value")))
				varNames <- c("param", "est", "se", "est_se", "pval")
			
			#Old 5-column standardized output from Mplus 4.2
			else if (identical(thisLine, c("Estimates", "S.E.", "Est./S.E.", "Std", "StdYX")))
				#in cases where combined raw and std, should split out results into list form
				varNames <- c("param", "est", "se", "est_se", "std", "stdyx")
			
			#Old 3-column output from Mplus 4.2
			else if (identical(thisLine, c("Estimates", "S.E.", "Est./S.E.")))
				#in cases where combined raw and std, should split out results into list form
				varNames <- c("param", "est", "se", "est_se")
					
			#MUML estimator or WLS estimators with covariates do not allow std. errors or StdY for standardized output
			#run 9.1b with MUML and OUTPUT:STANDARDIZED
			else if (identical(thisLine, c("StdYX", "Std")) && identical (nextLine, c("Estimate", "Estimate")))
				varNames <- c("param", "stdyx", "std")
			
			line <- line + 1
			if (exists("varNames"))
				detectionFinished <- TRUE
			else if (line > length(modelSection))
				stop("Unable to determine column names for model results section.")
			
		}
		return(varNames)
		
	}
	
	columnNames <- detectColumnNames(modelSection)
	
	#Detect model section dividers
	#These include: 1) multiple groups: Group XYZ
	#  2) latent classes: Latent Class XYZ
	#  3) two-level structure: Between Level, Within Level
	#  4) categorical latent variables: Categorical Latent Variables

	allSectionParameters <- c() #will hold extracted params for all sections

	betweenWithinMatches <- grep("^\\s*(Between|Within) Level\\s*$", modelSection, ignore.case=TRUE, perl=TRUE)
	latentClassMatches <- grep("^\\s*Latent Class (Pattern )*(\\d+\\s*)+$", modelSection, ignore.case=TRUE, perl=TRUE)
	multipleGroupMatches <- grep("^\\s*Group \\w+\\s*$", modelSection, ignore.case=TRUE, perl=TRUE)
	catLatentMatches <- grep("^\\s*Categorical Latent Variables\\s*$", modelSection, ignore.case=TRUE)
	
	topLevelMatches <- sort(c(betweenWithinMatches, latentClassMatches, multipleGroupMatches, catLatentMatches))
	
	if (length(topLevelMatches) > 0) {

		lcNum <- NULL
		bwWi <- NULL
		groupName <- NULL
		
		matchIndex <- 1
		for (match in topLevelMatches) {

			if (match %in% betweenWithinMatches) bwWi <- sub("^\\s*(Between|Within) Level\\s*$", "\\1", modelSection[match], perl=TRUE)
			else if (match %in% latentClassMatches) {
				if ((pos <- regexpr("Pattern", modelSection[match], ignore.case=TRUE)) > 0) {
					#need to pull out and concatenate all numerical values following pattern
					postPattern <- trimSpace(substr(modelSection[match], pos + attr(pos, "match.length"), nchar(modelSection[match])))
					#replace any spaces with periods to create usable unique lc levels
					lcNum <- gsub("\\s+", "\\.", postPattern, perl=TRUE)					
				}
				else lcNum <- sub("^\\s*Latent Class\\s+(\\d+)\\s*$", "\\1", modelSection[match], perl=TRUE)
			}
			else if (match %in% multipleGroupMatches) groupName <- sub("^\\s*Group (\\w+)\\s*$", "\\1", modelSection[match], perl=TRUE)
			else if (match %in% catLatentMatches) {
				#the categorical latent variables section is truly "top level"
				#that is, it starts over in terms of bw/wi and latent classes
				#multiple groups with cat latent variables is handled by knownclass and results in a latent class
				#pattern, so don't have to worry about nullifying groupName
				lcNum <- "Categorical.Latent.Variables"
				bwWi <- NULL
			}
			
			#if the subsequent top level match is more than 2 lines away, assume that there is a
			#chunk to be parsed. If it's <= 2, then assume that these are just blank lines
			chunkToParse <- FALSE
			if (matchIndex < length(topLevelMatches) && 
					(topLevelMatches[matchIndex + 1] - topLevelMatches[matchIndex]) > 2) {
				
				#extract all text between this match and the next one (add one to omit this header row,
				#subtract one to exclude the subsequent header row)
				thisChunk <- modelSection[(match+1):(topLevelMatches[matchIndex+1]-1)]
				chunkToParse <- TRUE				
			}
			else if (matchIndex == length(topLevelMatches)) {
				#also assume that the text following the last topLevelMatch is also to be parsed
				thisChunk <- modelSection[(match+1):length(modelSection)]
				chunkToParse <- TRUE
				
			}
			
			if (chunkToParse == TRUE) {
				parsedChunk <- extractParameters_1chunk(thisChunk, columnNames)
				
				#only append if there are some rows
				if (nrow(parsedChunk) > 0) {
					parsedChunk$LatentClass <- lcNum
					parsedChunk$BetweenWithin <- bwWi
					parsedChunk$Group <- groupName
					allSectionParameters <- rbind(allSectionParameters, parsedChunk)
				}				
			}
			
			matchIndex <- matchIndex + 1
		}
		

	}
	else allSectionParameters <- extractParameters_1chunk(modelSection, columnNames) #just one model section

	#if any std variable is one of the returned columns, we are dealing with an old-style combined results section (i.e.,
	#standardized results are not divided into their own sections, as with newer output).
	#newer output would just have the params, est, etc.
	#for consistency with newer output, we need to parse these into individual list elements and remove from unstandardized output.
	#this is a tricky maneuver in some ways because the function may return a data.frame or a list... will have to be handled by the caller
	oldStyleColumns <- c("stdyx", "stdy", "std")
	listParameters <- list()
	
	if (any(oldStyleColumns %in% names(allSectionParameters))) {

		#for each standardized column present, reprocess into its own df, append to list, and remove from the df
		for (colName in oldStyleColumns[oldStyleColumns %in% names(allSectionParameters)]) {
			listParameters[[paste(colName, ".standardized", sep="")]] <- data.frame(paramHeader=allSectionParameters$paramHeader,
					param=allSectionParameters$param, est=allSectionParameters[,colName], stringsAsFactors=FALSE)
			
			
			#also include latent class, multiple groups and bw/wi in the output
			if ("LatentClass" %in% names(allSectionParameters)) listParameters[[paste(colName, ".standardized", sep="")]]$LatentClass <- allSectionParameters$LatentClass
			if ("Group" %in% names(allSectionParameters)) listParameters[[paste(colName, ".standardized", sep="")]]$Group <- allSectionParameters$Group
			if ("BetweenWithin" %in% names(allSectionParameters)) listParameters[[paste(colName, ".standardized", sep="")]]$BetweenWithin <- allSectionParameters$BetweenWithin
			
			allSectionParameters[[colName]] <- NULL #remove from unstandardized output
			
			
		}
		listParameters[[sectionName]] <- allSectionParameters #now that standardized removed, add remainder to the list under appropriate name
	}
	else {
		#if output only contains results of one section type (stdyx, unstandardized, etc.),
		#then return a list with a single element, which will be appended to other elements by the extractParameters_1file function
		#copy data.frame into the appropriate list element to be returned.
		listParameters[[sectionName]] <- allSectionParameters
		
	} 

	return(listParameters)
	
	#a few examples of files to parse
	#mg + lc. Results in latent class pattern, not really different from regular latent class matching. See Example 7.21
	#mg + twolevel. Group is top, bw/wi is 2nd. See Example 9.11
	#lc + twolevel. Bw/wi is top, lc is 2nd. See Example 10.1. But categorical latent variables is even higher
	#test cases for more complex output: 7.21, 9.7, 9.11, 10.1   
}
	

extractParameters_1file <- function(filename, dropDimensions, resultType) {
	require(gsubfn)
	require(plyr)
	outfiletext <- scan(filename, what="character", sep="\n", strip.white=TRUE, blank.lines.skip=FALSE)
	
	if (length(grep("TYPE\\s+(IS|=|ARE)\\s+((MIXTURE|TWOLEVEL)\\s+)*EFA\\s+\\d+", outfiletext, ignore.case=TRUE, perl=TRUE)) > 0) {
		warning(paste("EFA, MIXTURE EFA, and TWOLEVEL EFA files are not currently supported by extractModelParameters.\n  Skipping outfile: ", filename, sep=""))
		return(NULL) #skip file
	}
		
	grabResultsSection <- function(sectionHeader, outfiletext) {
		#helper sub-function to extract a model section given a certain header.
		#the logic here is pretty convoluted. In general, Mplus results sections end with two blank lines
		#but there are problematic exceptions, like Example 9.7. Bengt has said that this formatting error will be fixed
		#in the next edition, but I've gone ahead and implemented a more nuanced (but excessively complicated) logic.
		
		#the end of the model results section is demarcated by two blank lines
		#this is not a reliable marker!! See Example 9.7. Breaks down with twolevel model.
		
		beginSection <- grep(sectionHeader, outfiletext)
		
		#if section header cannot be found, then bail out
		if (length(beginSection) == 0) return(NULL)
				
		endSection <- 0
		for (row in beginSection+1:length(outfiletext)) {
			#note short circuit && ensures that we will not go outside subscript bounds for outfiletext
			#check for current line and line+1 blank (two consecutive blank lines)
			if (row < length(outfiletext) && outfiletext[row] == "" && outfiletext[row+1] == "") {
				#given problems with example 9.7, also test that row+2 is a line of all capital letters
				#start by deleting all spaces
				capsLine <- gsub("\\s+", "", outfiletext[row+2], perl=TRUE)
				
				#now search for any non-capital letter (also allow for hyphens for R-SQUARE and numbers for TECHNICAL 1 OUTPUT)
				hasLowercase <- regexpr("[^0-9A-Z-]", capsLine, perl=TRUE) #will be -1 if all caps
				
				#actually, the caps check is breaking down for standardized output.
				#for stdyx, the stdy section is next, but begins with "STDY Standardization" (not all caps).
				#adding exception to logic below... getting kind of kludgy, but Mplus output is just not consistent.
				
				#if the next line is not all capitals, then continue reading output
				#even this could choke on a line like FACTOR BY, but that shouldn't happen because the logic requires two blank lines above
				if (hasLowercase < 0 || regexpr("STD[YX]*Standardization", capsLine, perl=TRUE) > 0) {
					endSection <- row
					break
				}
			}
		}
		
		if (!endSection > 0) stop("Could not locate results section end for header:\n  ", outfiletext[beginSection])
		
		modelSection <- outfiletext[(beginSection+1):(endSection-1)]
		
		return(modelSection)
		
	}
	
	#copy elements of append into target. note that data.frames inherit list, so could be wonky if append is a data.frame (shouldn't happen here)
	appendListElements <- function(target, append) {
		if (!is.list(target)) stop("target is not a list.")
		if (!is.list(append)) stop("append is not a list.")
		
		for (elementName in names(append)) {
			if (!is.null(target[[elementName]])) warning("Element is already present in target list: ", elementName)
			target[[elementName]] <- append[[elementName]]
		}
		
		return(target)
	}
	
	allSections <- list() #holds parameters for all identified sections

	unstandardizedSection <- grabResultsSection("^MODEL RESULTS$", outfiletext)
	if (!is.null(unstandardizedSection)) allSections <- appendListElements(allSections, extractParameters_1section(unstandardizedSection, "unstandardized"))
	
	beginStandardizedSection <- grep("^STANDARDIZED MODEL RESULTS$", outfiletext)
	
	if (length(beginStandardizedSection) > 0) {
		#check to see if standardized results are divided by standardization type (new format)
		remainder <- outfiletext[(beginStandardizedSection+1):length(outfiletext)]
		
		#could shift extractParameters_1section to receive the section header and pull out text there. Might streamline this.
		stdYXSection <- grabResultsSection("^STDYX Standardization$", remainder)
		if (!is.null(stdYXSection)) allSections <- appendListElements(allSections, extractParameters_1section(stdYXSection, "stdyx.standardized"))
		
		stdYSection <- grabResultsSection("^STDY Standardization$", remainder)
		if (!is.null(stdYSection)) allSections <- appendListElements(allSections, extractParameters_1section(stdYSection, "stdy.standardized"))
		
		stdSection <- grabResultsSection("^STD Standardization$", remainder)
		if (!is.null(stdSection)) allSections <- appendListElements(allSections, extractParameters_1section(stdSection, "std.standardized"))
		
		#if all individual standardized sections are absent, but the standardized section is present, must be old-style
		#combined standardized section (affects WLS and MUML, too). Extract and process old section. 
		if (all(is.null(stdYXSection), is.null(stdYSection), is.null(stdSection))) {
			oldStdSection <- grabResultsSection("^STANDARDIZED MODEL RESULTS$", outfiletext)
			if (!is.null(oldStdSection)) allSections <- appendListElements(allSections, extractParameters_1section(oldStdSection, "standardized")) #this section name should never survive the call
		}
	
	}
	
	listOrder <- c()
	if ("unstandardized" %in% names(allSections)) listOrder <- c(listOrder, "unstandardized")
	if ("stdyx.standardized" %in% names(allSections)) listOrder <- c(listOrder, "stdyx.standardized")
	if ("stdy.standardized" %in% names(allSections)) listOrder <- c(listOrder, "stdy.standardized")
	if ("std.standardized" %in% names(allSections)) listOrder <- c(listOrder, "std.standardized")
	
	#only re-order if out of order
	if(!identical(names(allSections), listOrder)) allSections <- allSections[listOrder] 

	#this needs to be here not to conflict with the drop to 1 element logic above.
	#if resultType passed (deprecated), only return the appropriate element
	#this is inefficient because all sections will be parsed, but it's deprecated, so no worries.
	if (!missing(resultType)) {
		warning(paste("resultType is deprecated and will be removed in a future version.\n  ",
						"extractModelParameters now returns a list containing unstandardized and standardized parameters, where available.\n  ",
						"For now, resultType is respected, so a data.frame will be returned."))

		oldNewTranslation <- switch(EXPR=resultType,
				"raw"="unstandardized",
				"stdyx"="stdyx.standardized",
				"stdy"="stdy.standardized",
				"std"="std.standardized")
		
		allSections <- allSections[[oldNewTranslation]]
	}
		
	return(allSections)
}

extractModelParameters <- function(target=getwd(), recursive=FALSE, filefilter, dropDimensions=FALSE, resultType) {

	#function tree (top to bottom):
	#extractModelParameters: loop over one or more output files
	#extractParameters_1file: extract model parameters for all sections (unstandardized, stdyx, stdy, std in a single file
	#extractParameters_1section: extract model parameters for a given section.
	#extractParameters_1chunk: extract model parameters for a given chunk (e.g., Latent class 2, Between Level) within a given section.
	
	outfiles <- getOutFileList(target, recursive, filefilter)
	
	allFiles <- list()
	for (curfile in outfiles) {
		#if not recursive, then each element is uniquely identified (we hope!) by filename alone
		if (recursive==FALSE)	listID <- make.names(splitFilePath(curfile)$filename) #each list element is named by the respective file
		else listID <- make.names(curfile) #each list element is named by the respective file
		
		allFiles[[listID]] <- extractParameters_1file(curfile, dropDimensions, resultType)
	}
	

	#dropDimensions <- TRUE
	if (length(allFiles) == 1) allFiles <- allFiles[[1]] # when only extracting a single file, return just the parameters list for the single model
	else if (dropDimensions == TRUE) {
		#in the case of multi-file output, we want to ensure that the interior lists (which contain model sections like stdyx.standardized)
		#all have a similar structure. But if all of them have only one element 
		allNames <- sapply(allFiles, names)
		allLengths <- sapply(allNames, length)
		
		#if there is only one unique name in the bunch and all sub-list lengths are 1, then collapse
		#could probably just check for one unique name.
		if (length(unique(unlist(allLengths))) == 1 && length(unique(unlist(allNames))) == 1) {
			allFiles <- sapply(allFiles, "[", 1)
		}
#		nameLengths <- sapply(allNames, length)
#		names(nameLengths) <- NULL
#		numUniqueLengths <- length(unique(nameLengths))
#		if (numUniqueLengths == 1) {
#			#all files in the model results list have the same number of elements
#			#need to check for identical names
#			
#		}
	}
	
	return(allFiles)
}	
	


#######
#EXPERIMENTAL CODE BELOW FOR GRAPHING MODELS
  
  
   
	


#test <- extractModelParameters("C:\\Program Files\\Mplus\\Mplus Examples\\User's Guide Examples\\ex5.1.out")

addNode <- function(dotModel, name, role, type) {
	if (!inherits(dotModel, "list")) stop("dotModel parameter must be a list")
	
	if (is.null(dotModel[[name]])) dotModel[[name]] <- list(role=role, type=type)
	else {
		#okay to convert something we thought was observed to latent (but not the other way
		if (dotModel[[name]]$type == "observed" && type == "latent") dotModel[[name]]$type <- type			

		#append the role if it's not already present (vars can have many roles)
		if (!role %in% dotModel[[name]]$role) dotModel[[name]]$role <- c(dotModel[[name]]$role, role) 
	}
	
	return(dotModel)
}

connectNodes <- function(dotModel, node1, node2, connectionType) {
	
}

graphModel <- function(model) {
	require(plyr)
	if (!inherits(model, "data.frame")) stop("Parameter model must be a data.frame")
	
	byOnWith <- grep("\\.(BY|ON|WITH)$", model$paramHeader, perl=TRUE)
	
	#create a df with name1, connectiontype, name2
	dotModel <- list(nodes=list(), connections=list())
	connections <- a_ply(model, 1, function(row) {
				splitHeader <- strsplit(row$paramHeader, ".", fixed=TRUE)[[1]]
				varName1 <- paste(splitHeader[-length(splitHeader)], collapse=".")
				connectType <- splitHeader[length(splitHeader)]
				varName2 <- row$param
				
				if (connectType == "ON") {
					dotModel$nodes <<- addNode(dotModel$nodes, name=varName1, role="outcome", type="observed")
					dotModel$nodes <<- addNode(dotModel$nodes, name=varName2, role="predictor", type="observed")
					dotModel$connections <<- connectNodes(dotModel$connections, varName1, varName2, "<-")
					
				}
				else if (connectType == "WITH") {
					dotModel <<- addNode(dotModel$nodes, name=varName1, role="covariance", type="observed")
					dotModel <<- addNode(dotModel$nodes, name=varName2, role="covariance", type="observed")
					dotModel$connections <<- connectNodes(dotModel$connections, varName1, varName2, "<->")
				}
				else if (connectType == "BY") {
					dotModel <<- addNode(dotModel$nodes, name=varName1, role="factor", type="latent")
					dotModel <<- addNode(dotModel$nodes, name=varName2, role="indicator", type="observed")
					dotModel$connections <<- connectNodes(dotModel$connections, varName1, varName2, "->")
				}
			})
	
#	varTypes <- ldply(strsplit(byOnWith, ".", fixed=TRUE), function(element) {
#				varName <- paste(element[-length(element)], collapse=".")
#				if (element[length(element)] == "BY") return(data.frame(name=varName, type="latent"))
#				else return(data.frame(name=varName, type="observed"))
#			})
#
#	
#	latentVars <- na.omit(unique(latentVars))
	
#	print(latentVars)
#	dotModel <- addNodes(list(), data.frame(name=latentVars, type="latent", stringsAsFactors=FALSE))
	
	return(dotModel)	
}