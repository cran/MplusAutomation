#Helper function for extractModelParameters. Used to parse each subsection of output within a given file and given results section (e.g., stdyx section) 
#There will be many chunks if latent classes, multiple groups, multilevel features are used.
extractParameters_1chunk <- function(thisChunk, columnNames) {
  if (missing(thisChunk) || is.na(thisChunk) || is.null(thisChunk)) stop("Missing chunk to parse.")
  if (missing(columnNames) || is.na(columnNames) || is.null(columnNames)) stop("Missing column names for chunk.")
  
  #okay to match beginning and end of line because strip.white used in scan
  matches <- gregexpr("^\\s*((Means|Thresholds|Intercepts|Variances|Residual Variances|New/Additional Parameters)|([\\w_\\d+\\.#]+\\s+(BY|WITH|ON|\\|)))\\s*$", thisChunk, perl=TRUE)
  
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
  
  
  #first trim all leading and trailing spaces (new under strip.white=FALSE)
  modelSection <- gsub("^\\s+|\\s+$", "", perl=TRUE, modelSection)
  
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


extractParameters_1file <- function(outfiletext, filename, resultType) {
  require(gsubfn)
  require(plyr)
  
  if (length(grep("TYPE\\s+(IS|=|ARE)\\s+((MIXTURE|TWOLEVEL)\\s+)+EFA\\s+\\d+", outfiletext, ignore.case=TRUE, perl=TRUE)) > 0) {
    warning(paste("EFA, MIXTURE EFA, and TWOLEVEL EFA files are not currently supported by extractModelParameters.\n  Skipping outfile: ", filename, sep=""))
    return(NULL) #skip file
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
  
  unstandardizedSection <- getMajorSection("^MODEL RESULTS$", outfiletext)
  if (!is.null(unstandardizedSection)) allSections <- appendListElements(allSections, extractParameters_1section(unstandardizedSection, "unstandardized"))
  
  beginStandardizedSection <- grep("^STANDARDIZED MODEL RESULTS$", outfiletext)
  
  if (length(beginStandardizedSection) > 0) {
    #check to see if standardized results are divided by standardization type (new format)
    remainder <- outfiletext[(beginStandardizedSection+1):length(outfiletext)]
    
    #could shift extractParameters_1section to receive the section header and pull out text there. Might streamline this.
    
    stdYXSection <- getMajorSection("^STDYX Standardization$", remainder)
    if (!is.null(stdYXSection)) allSections <- appendListElements(allSections, extractParameters_1section(stdYXSection, "stdyx.standardized"))
    
    stdYSection <- getMajorSection("^STDY Standardization$", remainder)
    if (!is.null(stdYSection)) allSections <- appendListElements(allSections, extractParameters_1section(stdYSection, "stdy.standardized"))
    
    stdSection <- getMajorSection("^STD Standardization$", remainder)
    if (!is.null(stdSection)) allSections <- appendListElements(allSections, extractParameters_1section(stdSection, "std.standardized"))
    
    #if all individual standardized sections are absent, but the standardized section is present, must be old-style
    #combined standardized section (affects WLS and MUML, too). Extract and process old section. 
    if (all(is.null(stdYXSection), is.null(stdYSection), is.null(stdSection))) {
      oldStdSection <- getMajorSection("^STANDARDIZED MODEL RESULTS$", outfiletext)
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
    
    outfiletext <- scan(curfile, what="character", sep="\n", strip.white=FALSE, blank.lines.skip=FALSE)
    
    allFiles[[listID]] <- extractParameters_1file(outfiletext, curfile, resultType)
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
