extractValue <- function(name, outfile, type="int") {
#extractValue(name, outfile, type="int")
#
#   name: the exact text to be matched in the outfile that identifies the parameter of interest
#   outfile: the Mplus output file to be parsed. Expects this as a vector of strings from the scan command.
#   type: the data type of the parameter, which determines the regexp used. Currently can be "int", "dec", "str", or "calc".
#
#   Description: An internal function used by modelParams to extract parameters from the output file using regular expressions.
#
  
  #locate the matching line in the output file
  matchlines <- grep(name, outfile, ignore.case=TRUE, value=TRUE)
  
  #using as.numeric for NAs to avoid mixed data types, which mess up the xtable creation
  
  #calc fields calculated based on values extracted, so they aren't directly in the output
  if (type == "calc") {
    return(as.numeric(NA))
  }
  
  if (length(matchlines) > 1) {
    warning(paste("More than one match found for parameter: ", name, sep=""))
    return(matchlines)
  }
  else if (length(matchlines) == 0) {
    #if the parameter of interest not found in this file, then return NA
    warning(paste("Parameter not found: ", name, sep=""))
    return(as.numeric(NA))
  }
  
  if (type == "int") {
    regexp <- "-*\\d+" #optional negative sign in front
  }
  else if (type == "dec") {
    #regexpr: -*\\d+\\.\\d+ : -* optional negative sign, \\d+ match at least one digit \\. match decimal sign \\d+ match decimal digits
    regexp <- "-*\\d+\\.\\d+" #optional negative sign in front
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
  else returnVal <- as.numeric(substr(matchlines[1], start, start + attr(start, "match.length")))
  
  return(returnVal)
}

modelParams <- function(outfile, filename, extract=c("Title", "LL", "BIC", "AIC", "AICC", "Params", "Observations", "BLRT"))
{
#modelParams(outfile, filename)
#   outfile: this is the output file in string form to be parsed. Passed in from parseModels.
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
  
  expandField <- function(name) {
    #internal function used to convert short keyword names for parameters into their full Mplus output equivalents
    #returns a vector of the exact string and the data type ("calc", "dec", "int", or "str").
    field <- switch(EXPR=name,
        Title = c("title: ", "str"),
        LL = c("H0 Value", "dec"),
        BIC = c("Bayesian \\(BIC\\)", "dec"),
        AIC = c("Akaike \\(AIC\\)", "dec"),
        AICC = c("AICC", "calc"),
        Params = c("Number of Free Parameters", "int"),
        aBIC = c("Sample-Size Adjusted BIC", "dec"),
        Entropy = c("Entropy", "dec"),
        Observations = c("Number of observations", "int"),
        CFI= c("CFI", "dec"),
        TLI = c("TLI", "dec"),
        c(name, "calc") #return untouched if did not match
    )
    
    return(field)
  }
  
  #convert keyword names into full strings for matching
  extractDetailed <- sapply(extract, expandField, USE.NAMES=F)
  
  #preallocates list
  arglist = vector("list", length(extract))  
  
  #iterate over each parameter to extract
  for (i in 1:length(extract)) {
    thisName <- extractDetailed[1,i]
    thisType <- extractDetailed[2,i]    
    arglist[i] <- extractValue(name=thisName, outfile, type=thisType)
  }
  
  #use the short keywords as the variable names	
  names(arglist) <- extract
  
  if ("LL" %in% extract) {
    if (is.na(arglist$LL)) {
      warning(paste("Model missing LL value. Likely a failed run. Dropping from data.\n  ", filename, sep=""))
      return(NULL)
    }
  }
  
  #handle AICC calculation, requires AIC, params, and observations
  if (all(c("AICC", "AIC", "Params", "Observations") %in% extract)) {
    #calculate adjusted AIC per Burnham & Anderson(2004), which is better than AIC for non-nested model selection
    arglist$AICC <- arglist$AIC + (2*arglist$Params*(arglist$Params+1))/(arglist$Observations-arglist$Params-1)  
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
      
      arglist$KM1LL <- extractValue(name="H0 Loglikelihood Value", blrtpiece, type="dec")
      arglist$BLRTp <- extractValue(name="Approximate P-Value", blrtpiece, type="dec")
      arglist$BLRTNumdraws <- extractValue(name="Successful Bootstrap Draws", blrtpiece, type="int")
    }
    else {
      #warning("Could not locate BLRT section, despite being requested")
      
      #need to pad the expected fields with NAs to keep the list length consistent, permitting correct rbind
      arglist$KM1LL <- as.numeric(NA)
      arglist$BLRTp <- as.numeric(NA)
      arglist$BLRTNumdraws <- as.numeric(NA)
    }
  }
  
  #arglist$inputInstructions <- as.character(outfile[(startInput+1):(endInput-1)])
  arglist$inputInstructions <- paste((outfile[(startInput+1):(endInput-1)]), collapse="\n")
  
  return(as.data.frame(arglist, stringsAsFactors=FALSE))
} 

parseModels <- function(directory=getwd(), recursive=FALSE, filefilter) {
#parseModels(directory, recursive=FALSE)
#
#   directory: the directory containing Mplus output files to read. Use forward slashes in directory name (e.g., "C:/Users/Mplus/"). Defaults to working directory.
#   recursive: specifies whether to parse output files in subdirectories beneath the specified directory. Defaults to FALSE. (TRUE or FALSE)
#
#   Description: This function identifies all Mplus .out files in the specified directory (directory parameter)
#   and reads basic model fit information from each file. The function combines fit details across models into a list.
#
#   Example: myModels <- parseModels("C:/Documents and Settings/Michael/My Documents/Mplus Stuff/", recursive=TRUE)
  
  #retain working directory and reset at end of run
  curdir <- getwd()
  setwd(directory)
  
  #obtain list of all files in the specified directory
  filelist <- list.files(recursive=recursive)
  
  #retain only .out files
  outfiles <- filelist[grep(".*\\.out", filelist, ignore.case=TRUE)]
  
  if (!missing(filefilter)) {
    dropOutExtensions <- sapply(outfiles, function(x) {
          if (nchar(x) >= 4) return(tolower(substr(x, 1, (nchar(x)-4))))
        })
    outfiles <- outfiles[grep(paste(".*", filefilter, ".*", sep=""), dropOutExtensions, ignore.case=TRUE, perl=TRUE)]
  }
  
  if (length(outfiles) == 0) { 
    warning("No output files detected in this directory.")
    return(NULL)
  }
  
  details <- c()
  
  #for each output file, use the modelParams function to extract relevant data
  #note that modelParams returns data as a list
  #rbind creates an array of lists by appending each modelParams retun value
  for (i in 1:length(outfiles)) {
    #read the file
    readfile <- scan(outfiles[i], what="character", sep="\n", strip.white=TRUE)
    
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
  
  variableWidths <- strapply(variableFormats, "[EFG]+(\\d+)\\.\\d+", as.numeric, perl=TRUE, simplify=TRUE)
  
  #trim leading and trailing space from the filename
  fileName <- sub("^\\s*","", savedataSection[saveFileStart+1], perl=TRUE)
  fileName <- sub("\\s*$","", fileName, perl=TRUE)
  
  #return the file information as a list
  return(list(fileName=fileName, varNames=variableNames, varFormats=variableFormats, varWidths=variableWidths))
  
}

createTable <- function(modelList, filename=file.path(getwd(), "Model Comparison.html"),
  sortby="AICC", display=TRUE, latex=FALSE, dropCols=c("inputInstructions", "Observations"), label=NULL) {
#createTable(directory, recursive=FALSE)
#
#   modelList: list of model details returned by parseModels.
#   basedir: directory in which to save the HTML table. Defaults to current directory.
#   filename: name of HTML table file. Defaults to model comparison.html
#   sortby: name of field on which to sort. Defaults to "AICC". "BIC" and "AIC" are options.
#   display: whether to load the HTML table in the browser after creating it. Defaults to TRUE. (TRUE/FALSE) 
#
#   Description: This function generates an HTML table from a list of models generated by parseModels.
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

extractModelResults <- function(outfile, resultType="raw") {
  require(gsubfn)
  require(plyr)
  readfile <- scan(outfile, what="character", sep="\n", strip.white=TRUE, blank.lines.skip=FALSE)
  
  #locate the start of the model results section
  #note that this won't work for EFA... but do I care? :)
  if (resultType=="raw") beginModel <- grep("^MODEL RESULTS$", readfile)
  else if (resultType=="stdyx") beginModel <- grep("^STDYX Standardization$", readfile)
  else if (resultType=="stdy") beginModel <- grep("^STDY Standardization$", readfile)
  else if (resultType=="std") beginModel <- grep("^STD Standardization$", readfile)
  else stop("Unsupported result type. Must be one of: \"raw\", \"stdyx\", \"stdy\", \"std\"")
  
  #the end of the model results section is demarcated by two blank lines
  endModel <- 0
  for (row in beginModel+1:length(readfile)) {
    #note short circuit && ensures that we will not go outside subscript bounds for readfile
    if (row < length(readfile) && readfile[row] == "" && readfile[row+1] == "") {
      endModel <- row
      break
    }
  }
  #should be one exact match for beginModel and non-zero endModel
  stopifnot(length(beginModel)==1, endModel > 0)
  
  #these are here to warn for non-standard models (so that the code can be made more flexible in the future)
  #may need to tweak for Latent Class models
  if (!readfile[beginModel+1]=="") warning("no blank line following MODEL RESULTS")
  if (!readfile[beginModel+2]=="Two-Tailed") warning("model results + 2 != two-tailed")
  if (!regexpr("^\\s*Estimate\\s+S\\.E\\.\\s+Est\\./S\\.E\\.\\s+P-Value\\s*$",readfile[beginModel+3],perl=TRUE) > 0) {
    warning("model results + 3 is not the estimate s.e. line")
  }
  
  #select the model section for further processing
  modelSection <- readfile[(beginModel+1):(endModel-1)]

  #helper function used to parse each chunk of output (will be many if latent classes are used)
  parseChunk <- function(thisChunk) {
    matches <- gregexpr("^\\s*((Means|Thresholds|Intercepts|Variances|Residual Variances)|([\\w_\\d+\\.]+\\s+(BY|WITH|ON|\\|)))\\s*$", thisChunk, perl=TRUE)
    #cbind together the start and end matches for each line of the gregexpr list
    #then rbind together all of the start and end lines to create a matrix
    #convertMatches <- do.call("rbind", lapply(matches, function(x) cbind(x, attr(x, "match.length"))))
    #now convert it to a data frame with the line number included
    #convertMatches2 <- data.frame(line=1:nrow(convertMatches),start=convertMatches[,1], end=convertMatches[,2])
    
    #more readable (than above) using ldply from plyr
    convertMatches <- ldply(matches, function(row) data.frame(start=row, end=attr(row, "match.length")))
    convertMatches$startline <- 1:nrow(convertMatches)
    
    #only keep lines with a single match
    #this removes rows that are -1 from gregexpr
    convertMatches <- subset(convertMatches, start > 0)
    
    #develop a dataframe that divides into keyword matches versus variable matches
    convertMatches <- ddply(convertMatches, "startline", function(row) {
          #pull the matching keyword based on the start/end attributes from gregexpr
          match <- substr(thisChunk[row$startline], row$start, row$end)
          #check for keyword
          if (match %in% c("Means", "Thresholds", "Intercepts", "Variances", "Residual Variances")) {
            return(data.frame(startline=row$startline, keyword=make.names(match), varname=NA_character_, operator=NA_character_))
          }
          else if (length(variable <- strapply(match, "^\\s*([\\w_\\d+\\.]+)\\s+(BY|WITH|ON|\\|)\\s*$", c, perl=TRUE)[[1]]) > 0) {
            return(data.frame(startline=row$startline, keyword=NA_character_, varname=variable[1], operator=variable[2]))
          }
          else stop("failure to match keyword: ", match)
        })
    
    comboFrame <- c()
    for (i in 1:nrow(convertMatches)) {
      if (i < nrow(convertMatches)) convertMatches[i,"endline"] <- convertMatches[i+1,"startline"]-1
      else convertMatches[i,"endline"] <- length(thisChunk)
      
      chunk <- thisChunk[convertMatches[i, "startline"]:convertMatches[i, "endline"]]
      
      chunkParsed <- strapply(chunk, "^\\s*(\\w+[\\w_\\d+\\.\\$#]+)\\s+([-\\d\\.]+)\\s+([-\\d\\.]+)\\s+([-\\d\\.]+)\\s+([-\\d\\.]+)\\s*$", 
          function(varmatch, est, se, est_se, pval) {
            if (is.na(convertMatches[i,]$keyword)) varTitle <- paste(convertMatches[i,]$varname, ".", convertMatches[i,]$operator, sep="")
            else varTitle <- as.character(convertMatches[i,]$keyword)
            
            #return(list(paramHeader=varTitle, param=varmatch, est=as.numeric(est), 
            #se=as.numeric(se), est_se=as.numeric(est_se), pval=as.numeric(pval)))
            
            return(data.frame(paramHeader=varTitle, param=varmatch, est=as.numeric(est), 
                    se=as.numeric(se), est_se=as.numeric(est_se), pval=as.numeric(pval), stringsAsFactors=FALSE))
          },
          perl=TRUE#, simplify=data.frame
      )
      
      #strapply will return a list of data frames
      chunkParsed <- do.call("rbind", chunkParsed)
      
      #add the current chunk to the overall data.frame
      comboFrame <- rbind(comboFrame, chunkParsed)
      
    }
    
    #so, the combination of rbind and list returns from strapply
    #yields a list stored as a matrix with dimnames and dims
    #just doing as.data.frame results in a DF with elements that are lists themselves
    #this is dumb. we just want a regular DF.
    #may be able to improve this later, but for now we have to kludge
    #the dataframe using lapply
    #to unlist each column, then reassemble with data.frame
    return(data.frame(lapply(data.frame(comboFrame), unlist)))
    
  }
  
  
  #check for latent classes
  latentClassMatches <- grep("^\\s*Latent Class \\d+\\s*$", modelSection, ignore.case=TRUE, perl=TRUE)
  
  if (length(latentClassMatches) > 0) {
    #if there are latent class sections, read these one at a time.
    #otherwise, just parse the whole section and return it
    
    bigFrame <- c()
    catVars <- FALSE
    for (i in 1:length(latentClassMatches)) {
      if (i < length(latentClassMatches)) thisChunk <- modelSection[(latentClassMatches[i]+1):(latentClassMatches[i+1]-1)]
      else if (i == length(latentClassMatches)) {
        #check for use of categorical latent vars, which have their own section
        if (length(catPos <- grep("Categorical Latent Variables", modelSection)) > 0) {
          thisChunk <- modelSection[(latentClassMatches[i]+1):(catPos-1)] 
          catVars <- TRUE
        }
        else thisChunk <- modelSection[(latentClassMatches[i]+1):length(modelSection)]
      } 
      
      parsedChunk <- parseChunk(thisChunk)
      lcNum <- sub("^\\s*Latent Class (\\d+)\\s*$", "\\1", modelSection[latentClassMatches[i]], perl=TRUE)
      parsedChunk$LatentClass <- lcNum
      bigFrame <- rbind(bigFrame, parsedChunk)
      
      if (catVars == TRUE) {
        catChunk <- modelSection[(catPos+1):length(modelSection)]
        catParsed <- parseChunk(catChunk)
        catParsed$LatentClass <- "CatVars"
        bigFrame <- rbind(bigFrame, catParsed)
      }
    }
    
    return(bigFrame)
  }
  else return(parseChunk(modelSection))
  
#useful command for full-text searching within r source files:
#find ./ -iname "*.r" -print0 | xargs -0 grep "match.length"
#need print0 and -0 to null-terminate find results, which allows for spaces in file names  
}
