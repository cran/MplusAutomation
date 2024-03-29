#' Extract Modification Indices for One Chunk (Section)
#'
#' To do: add details.
#'
#' @param chunk character vector of current subsection from which to extract mod indices
#' @param columnNames character vector of expected column (variable) names for section
#' @param filename name of Mplus output file being parsed
#' @return A data frame
#' @keywords internal
extractModIndices_1chunk <- function(chunk, columnNames, filename) {
  #only search for lines with actual MI data, not headers or blank lines
  #remove spaces within mean/intercept brackets so that regexp works properly (treats the [I1] as a single var)
  chunk <- gsub("\\[\\s*([^\\s]+)\\s*\\]", "[\\1]", chunk, perl=TRUE)
  matches <- friendlyGregexpr("^\\s*([\\w_\\d+\\.#]+\\s+(BY|WITH|ON)\\s+[\\w_\\d+\\.#]+).*$", chunk)
  matches <- rbind(matches, friendlyGregexpr("^\\s*\\[[\\s\\w\\d\\.#\\$]+\\].*$", chunk)) #separate regexp for means/ints (just for ease of reading)
  
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
        thisLine[4L] == "/" &&
        length(nextLine) > 4 &&
        nextLine[1L] == thisLine[3L] && #var1 of ON line corresponds to var2 of BY line
        nextLine[3L] == thisLine[1L] #var2 of ON line corresponds to var1 of BY line
        ) {
      splitParams[[line]][4:length(nextLine)] <- nextLine[4:length(nextLine)]
    } else if (length(thisLine) < numCols && substr(thisLine[1], 1, 1) == "[") {
      splitParams[[line]] <- c(thisLine[1], "NA", "NA", thisLine[2:length(thisLine)]) #there is no V2 or operator here, so just pad with NA (technically this stays as char in output)
    }
      
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


#' Extract Modification Indices for One File
#'
#' To do: add details.
#'
#' @param outfiletext character vector of Mplus output file being processed
#' @param filename name of Mplus output file being processed
#' @return A data frame
#' @keywords internal
extractModIndices_1file <- function(outfiletext, filename) {

  if (isEmpty(outfiletext)) stop("Missing mod indices to parse.\n  ", filename)

  MISection <- getSection("^MODEL MODIFICATION INDICES$", outfiletext)

  if (is.null(MISection)) return(NULL) #model does not contain modindices

  MISection <- gsub("(^\\s+|\\s+$)", "", MISection, perl=TRUE)
  columnNames <- detectColumnNames(filename, MISection, "mod_indices")
  if (all(is.na(columnNames)) || is.null(columnNames)) stop("Missing column names for mod indices.\n  ", filename)

  #support multiple group output
  multipleGroupMatches <- grep("^\\s*Group \\w+\\s*$", MISection, ignore.case=TRUE, perl=TRUE)
  
  if (length(multipleGroupMatches) > 0L) {
    matchIndex <- 1
    allSectionMI <- c()
    for (match in multipleGroupMatches) {
      groupName <- sub("^\\s*Group (\\w+)\\s*$", "\\1", MISection[match], perl=TRUE)

      chunkToParse <- FALSE
      if (matchIndex < length(multipleGroupMatches) &&
          (multipleGroupMatches[matchIndex + 1] - multipleGroupMatches[matchIndex]) > 2) {
        
        #extract all text between this match and the next one (add one to omit this header row,
        #subtract one to exclude the subsequent header row)
        thisChunk <- MISection[(match+1):(multipleGroupMatches[matchIndex+1]-1)]
        chunkToParse <- TRUE
      } else if (matchIndex == length(multipleGroupMatches) && match+1 <= length(MISection)) {
        #also assume that the text following the last multipleGroupMatch is also to be parsed
        #second clause ensures that there is some chunk below the final header.
        #this handles issues where a blank section terminates the results section, such as multilevel w/ no between
        thisChunk <- MISection[(match+1):length(MISection)]
        chunkToParse <- TRUE
      }
      
      if (chunkToParse) {
        parsedChunk <- extractModIndices_1chunk(thisChunk, columnNames, filename)
        
        #only append if there are some rows
        if (!is.null(parsedChunk) && nrow(parsedChunk) > 0) {
          parsedChunk$Group <- groupName
          allSectionMI <- rbind(allSectionMI, parsedChunk)
        }
      }
      matchIndex <- matchIndex + 1
    }
  } else {
    #just one section to parse
    allSectionMI <- extractModIndices_1chunk(MISection, columnNames, filename)
  }
  
  return(allSectionMI)
}

#' Extract model modification indices.
#'
#' Extracts the model modification indices from the MODEL MODIFICATION INDICES section of one
#' or more Mplus output files. If the \code{target} is a directory, all .out files therein will be parsed
#' and a single list will be returned, where the list elements are named by the output file name.
#' Returned parameters typically include the pairwise relationships between variables to be freed,
#' the change in model chi-square (M.I.), and the expected parameter change (E.P.C.).
#'
#' @param target the directory containing Mplus output files (.out) to parse OR
#'   the single output file to be parsed. May be a full path, relative path, or
#'   a filename within the working directory. Defaults to the current working
#'   directory. Example: \dQuote{C:/Users/Michael/Mplus Runs}
#' @param recursive optional. If \code{TRUE}, parse all models
#'   nested in subdirectories within \code{target}. Defaults to
#'   \code{FALSE}.
#' @param filefilter a Perl regular expression (PCRE-compatible)
#'   specifying particular output files to be parsed within \code{directory}.
#'   See \code{regex} or \url{http://www.pcre.org/pcre.txt} for
#'   details about regular expression syntax.
#'
#' @return If \code{target} is a single file, a data.frame
#'   containing modification results for the target output file will be
#'   returned. If \code{target} is a directory, a list will be returned,
#'   where each element contains a data.frame of the modification indices
#'   for a single file, and the top-level elements are named after the
#'   corresponding output file name. The basic \code{data.frame} containing
#'   the MODEL MODIFICATION INDICES section of \code{outfile}. Variables include
#'   \item{modV1}{The first variable in the pair to be freed according to the M.I.}
#'   \item{operator}{The suggested relationship between \code{modV1} and \code{modV2}
#'     (e.g., WITH for freeing the covariance between \code{modV1} and \code{modV2})}
#'   \item{modV2}{The first variable in the pair to be freed according to the M.I.}
#'   \item{MI}{The decrease in model chi-square if the specified relationship is freely estimated}
#'   \item{EPC}{The expected parameter estimate between \code{modV1} and \code{modV2} if freed.}
#'   \item{Std_EPC}{The EPC value standardized using the variances of the continuous latent variables.}
#'   \item{StdYX_EPC}{The EPC value standardized using the variances of the continuous latent
#'     variables as well as the variances of the background and/or outcome variables.}
#' @author Michael Hallquist
#' @export
#' @seealso \code{\link{readModels}}, \code{\link{extractModelSummaries}}, \code{\link{extractModelParameters}}
#' @keywords interface
#' @examples
#' \dontrun{
#' ex3.14 <- extractModIndices(
#' 	"C:/Program Files/Mplus/Mplus Examples/User's Guide Examples/ex3.14.out")
#' }
extractModIndices <- function(target=getwd(), recursive=FALSE, filefilter) {
  outfiles <- getOutFileList(target, recursive, filefilter)

  allFiles <- list()

  for (curfile in outfiles) {
    #if not recursive, then each element is uniquely identified (we hope!) by filename alone
    if (recursive==FALSE) {
      listID <- make.names(splitFilePath(curfile)$filename) #each list element is named by the respective file
    } else {
      listID <- make.names(curfile) #each list element is named by the respective file
    }

    outfiletext <- scan(curfile, what="character", sep="\n", strip.white=FALSE, blank.lines.skip=FALSE, quiet=TRUE)

    allFiles[[listID]] <- extractModIndices_1file(outfiletext, curfile)
  }


  #dropDimensions <- TRUE
  if (length(allFiles) == 1) allFiles <- allFiles[[1]] # when only extracting a single file, return just the MI df for the single model

  return(allFiles)

}
