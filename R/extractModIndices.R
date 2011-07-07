extractModIndices_1file <- function(outfiletext, filename) {

  if (missing(outfiletext) || is.na(outfiletext) || is.null(outfiletext)) stop("Missing mod indices to parse.\n  ", filename)
    
  MISection <- getSection("^MODEL MODIFICATION INDICES$", outfiletext)
  
  if (is.null(MISection)) return(NULL) #model does not contain modindices
  
  MISection <- gsub("(^\\s+|\\s+$)", "", MISection, perl=TRUE)
  columnNames <- detectColumnNames(filename, MISection, "mod_indices")
  
  if (is.na(columnNames) || is.null(columnNames)) stop("Missing column names for mod indices.\n  ", filename)
  
  #only search for lines with actual MI data, not headers or blank lines
  matches <- friendlyGregexpr("^\\s*([\\w_\\d+\\.#]+\\s+(BY|WITH|ON)\\s+[\\w_\\d+\\.#]+).*$", MISection)
  
  if (is.null(matches)) return(NULL) #model contains the MI section, but no values. Occurs when no MI reaches above min threshold (e.g., 5)
  
  splitParams <- strsplit(matches$tag, "\\s+", perl=TRUE)
  numCols <- length(columnNames)
  for (line in 1:length(splitParams)) {
    thisLine <- splitParams[[line]]
    if (line < length(splitParams)) nextLine <- splitParams[[line+1]]
    else nextLine <- NULL
    
    #in the on/by section, the on comes first with a trailing / and no data, followed by the by line with the MI data
    #check for this and propagate data from subsequent row to this row
    if (length(thisLine) < numCols && length(thisLine) >= 4 &&
        thisLine[4] == "/" && 
        length(nextLine) > 4 && 
        nextLine[1] == thisLine[3] && #var1 of ON line corresponds to var2 of BY line
        nextLine[3] == thisLine[1] #var2 of ON line corresponds to var1 of BY line
    )
      splitParams[[line]][4:length(nextLine)] <- nextLine[4:length(nextLine)] 
  }
  
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
 
  return(parsedParams)
}
  
extractModIndices <- function(target=getwd(), recursive=FALSE, filefilter) {
  outfiles <- getOutFileList(target, recursive, filefilter)
  
  allFiles <- list()
  
  for (curfile in outfiles) {
    #if not recursive, then each element is uniquely identified (we hope!) by filename alone
    if (recursive==FALSE)	listID <- make.names(splitFilePath(curfile)$filename) #each list element is named by the respective file
    else listID <- make.names(curfile) #each list element is named by the respective file
    
    outfiletext <- scan(curfile, what="character", sep="\n", strip.white=FALSE, blank.lines.skip=FALSE)
    
    allFiles[[listID]] <- extractModIndices_1file(outfiletext, curfile)
  }
  
  
  #dropDimensions <- TRUE
  if (length(allFiles) == 1) allFiles <- allFiles[[1]] # when only extracting a single file, return just the MI df for the single model
  
  return(allFiles)
  
}