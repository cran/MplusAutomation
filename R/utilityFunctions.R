#expose as root-level function to be used by model summary extraction
getMajorSection <- function(sectionHeader, outfiletext) {
  #helper sub-function to extract a model section given a certain header.
  #the logic here is pretty convoluted. In general, Mplus results sections end with two blank lines
  #but there are problematic exceptions, like Example 9.7. Bengt has said that this formatting error will be fixed
  #in the next edition, but I've gone ahead and implemented a more nuanced (but excessively complicated) logic.
  
  #the end of the model results section is demarcated by two blank lines
  #this is not a reliable marker!! See Example 9.7. Breaks down with twolevel model.
  
  #27Aug2010: beginSection may match multiple headers and end up with length > 1.
  #Example: STDYX Standardization appears in the standardized model results section
  #and in the total, direct, and indirect sections.
  #this results in a screwy looping sequence below that will not work.
  #solution: just use the first element (most proximal match)
  beginSection <- grep(sectionHeader, outfiletext)[1]
  
  #if section header cannot be found, then bail out
  if (is.na(beginSection)) return(NULL)
  
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
  
  
  #considering another approach that just identifies a fixed
  #number of top-level headers
  #topLevelHeaders <- c("INPUT INSTRUCTIONS", "SUMMARY OF ANALYSIS",
  #  "SUMMARY OF DATA FOR THE FIRST DATA SET", "SAMPLE STATISTICS", "TESTS OF MODEL FIT",
  #  "CLASSIFICATION QUALITY", "CLASSIFICATION OF INDIVIDUALS BASED ON THEIR MOST LIKELY LATENT CLASS MEMBERSHIP",
  #  "MODEL RESULTS", "LOGISTIC REGRESSION ODDS RATIO RESULTS", "STANDARDIZED MODEL RESULTS", 
  #  "R-SQUARE", "QUALITY OF NUMERICAL RESULTS", "TECHNICAL \\d+ OUTPUT", "TOTAL, TOTAL INDIRECT, SPECIFIC INDIRECT, AND DIRECT EFFECTS",
  #  "STANDARDIZED TOTAL, TOTAL INDIRECT, SPECIFIC INDIRECT, AND DIRECT EFFECTS", "RESIDUAL OUTPUT", "MODEL MODIFICATION INDICES")
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

#helper function
splitFilePath <- function(abspath) {
  #function to split absolute path into path and filename 
  #code adapted from R.utils filePath command
  if (!is.character(abspath)) stop("Path not a character string")
  if (nchar(abspath) < 1 || is.na(abspath)) stop("Path is missing or of zero length")
  
  components <- strsplit(abspath, split="[\\/]")[[1]]
  lcom <- length(components)
  
  stopifnot(lcom > 0)
  
  #the file is the last element in the list. In the case of length == 1, this will extract the only element.
  relFilename <- components[lcom]
  
  if (lcom == 1) {
    dirpart <- NA_character_
  }
  else if (lcom > 1) {
    #drop the file from the list (the last element)
    components <- components[-lcom]
    dirpart <- do.call("file.path", as.list(components)) 
  }
  
  return(list(directory=dirpart, filename=relFilename))
}
