readModels <- function(target=getwd(), recursive=FALSE, filefilter) {
	#large wrapper function to read summaries, parameters, and savedata from one or more output files.
	
	outfiles <- getOutFileList(target, recursive, filefilter)
	
	allFiles <- list()
	for (curfile in outfiles) {
		#if not recursive, then each element is uniquely identified (we hope!) by filename alone
		if (recursive==FALSE)	listID <- make.names(splitFilePath(curfile)$filename) #each list element is named by the respective file
		else listID <- make.names(curfile) #each list element is named by the respective file

		outfiletext <- scan(curfile, what="character", sep="\n", strip.white=FALSE, blank.lines.skip=FALSE)
				
		allFiles[[listID]]$parameters <- extractParameters_1file(outfiletext, curfile)
		#allFiles[[listID]]$modification.indices <- 
		allFiles[[listID]]$savedata <- l_getSavedata_Data(curfile, outfiletext)
		allFiles[[listID]]$summaries <- extractSummaries_1file(outfiletext, curfile)
    
    #cleanup summary columns containing only NAs
    for (col in names(allFiles[[listID]]$summaries)) {
      if (all(is.na(allFiles[[listID]]$summaries[[col]]))) allFiles[[listID]]$summaries[[col]] <- NULL
    }
    
	}

  if (length(outfiles)==1)
    allFiles <- allFiles[[1]] #no need for single top-level element when there is only one file 
  
  return(allFiles)		
}

extractValue <- function(pattern, textToScan, filename, type="int") {
#extractValue(pattern, outfile, type="int")
#
#   pattern: the exact text to be matched in the outfile that identifies the parameter of interest
#   textToScan: the chunk of Mplus output to be parsed, passed as a vector of character strings (from the scan command).
#		filename: the name of the file containing textToScan. Used to make more intelligible warning messages.
#   type: the data type of the parameter, which determines the regexp used. Currently can be "int", "dec", "str", or "calc".
#
#   Description: An internal function used by extractSummaries_1file to extract parameters from the output file using regular expressions.
#
  
  #locate the matching line in the output file
  matchlines <- grep(pattern, textToScan, ignore.case=TRUE, value=TRUE)
  
  if (length(matchlines) > 1) {
		stop("More than one match found for parameter: ", pattern, "\n  ", filename)
    #return(matchlines) #not sure what I was thinking here... seems better to stop than warn and return lines
  }
  else if (length(matchlines) == 0) {
    #if the parameter of interest not found in this file, then return NA
    #warning(paste("Parameter not found: ", pattern, "\n  ", filename, sep=""))
    if (type == "int") return(NA_integer_)
    else if (type == "dec") return(NA_real_)
    else if (type == "str") return(NA_character_)
  }
  
  #different idea: concatenate pattern with var type and match on that
  #then sub just the pattern part from the larger line
  
  typePrefix <- substr(type, 1, 3)
  
  if (typePrefix == "int") {
    regexp <- "-*\\d+" #optional negative sign in front
  }
  else if (typePrefix == "dec") {
    #regexpr: -*\\d+\\.\\d+ : -* optional negative sign, \\d+ match at least one digit \\. match decimal sign \\d+ match decimal digits
    regexp <- "-*\\d+\\.\\d+"
  }
  else if (typePrefix == "str") {
    regexp <- paste(pattern, ".*", sep="")
  }
  
  #locate the match
  valueMatches <- gregexpr(regexp, matchlines[1], perl=TRUE)[[1]]
  
  if (type == "str") {
    #remove the tag portion of the string (e.g., "title:"), retaining rest of line
    returnVal <- as.character(sub(pattern, "", matchlines[1], ignore.case=TRUE))    
  }
  else {
    #excessively tight syntax: replace dec[15] with 15, if number at end of type. Otherwise return just "dec".
    #then grep result for only numeric characters (\\d+). If grep is false (i.e., no numerals in substitution,
    #then no index was specified in type, so type must be simply "dec", "int", or "str" (as opposed to "int[15]"), so set as 1
    if (!grepl("^\\d+$", whichMatch <- sub("^.*\\[(\\d+)\\]$", "\\1", type, perl=TRUE), perl=TRUE)) whichMatch <- 1
    else whichMatch <- as.numeric(whichMatch)
    
    #pull from the start of the match through match.length, which is the length of characters that matched
    #need to subtract one from the start + length offset to grab the correct number of characters
    #(e.g., if the match runs from 40-44, the start will be 40, with length 5, but 40 + 5 would be 6 characters, hence -1 
    returnVal <- as.numeric(substr(matchlines[1], valueMatches[whichMatch], valueMatches[whichMatch] + attr(valueMatches, "match.length")[whichMatch] - 1))
    
  }
  
  return(returnVal)
}

#new approach to multiline section: retain spaces and look for next
#line that has identical indentation.
getMultilineSection <- function(header, outfiletext, filename) {
	
	#allow for multiple depths (subsections) separated by ::
	header <- strsplit(header, "::", fixed=TRUE)[[1]]
	
	targetText <- outfiletext
	for (level in 1:length(header)) {
		headerRow <- grep(paste("^\\s*", header[level], "\\s*$", sep=""), targetText, perl=TRUE)
		
		if (length(headerRow) == 1) {
			
			#locate the position of the first non-space character
			numSpacesHeader <- regexpr("\\S+.*$", targetText[headerRow], perl=TRUE) - 1
			
			sectionStart <- headerRow + 1 #skip header row itself
			#if (outfiletext[sectionStart] == "") sectionStart <- sectionStart + 1 #As far as I know, there is always a blank line after the header, so skip past it
			
			sameLevelMatch <- FALSE
			readStart <- sectionStart #counter variable to chunk through output
			while(sameLevelMatch == FALSE) {
				#read 20-line chunks of text to find next line with identical identation
				#more efficient than running gregexpr on whole output
				#match position of first non-space character, subtract 1 to get num spaces.
				#blank lines will generate a value of -2, so shouldn't throw off top-level match
				firstNonspaceCharacter <- lapply(gregexpr("\\S+.*$", targetText[readStart:(readStart+19)], perl=TRUE), FUN=function(x) x - 1)
				samelevelMatches <- which(firstNonspaceCharacter == numSpacesHeader)
				if (length(samelevelMatches) > 0) {
					sameLevelMatch <- TRUE
					sectionEnd <- readStart+samelevelMatches[1] - 2 #-1 for going to line before next header, another -1 for readStart
				}
        else if (readStart+19 >= length(targetText)) {
          sameLevelMatch <- TRUE
          sectionEnd <- length(targetText)
        }
				else readStart <- readStart + 20 #process next batch

        #if (readStart > 100000) browser()#stop ("readStart exceeded 100000. Must be formatting problem.")
			}
			
			#set targetText as chunk from start to end. If there are multiple subsections, then the
			#next iteration of the for loop will process within the subsetted targetText.
			targetText <- targetText[sectionStart:sectionEnd]
			
		}
		else {
			targetText <- NA_character_
			if (length(headerRow) > 1) warning(paste("Multiple matches for header: ", header, "\n  ", filename, sep=""))
			break
			#else if (length(headerRow) < 1) warning(paste("Could not locate section based on header: ", header, "\n  ", filename, sep=""))
		}
		
	}
	 
  return(targetText)
}


#sectionHeaders is a character vector with headers for each section of interest
#sectionFields is a list of data.frames where each data.frame specifies the fields to be extracted for that section
#make this a more generic function that accepts headers and fields in case it is useful outside the MODEL FIT section
extractSummaries_1plan <- function(arglist, sectionHeaders, sectionFields, textToParse, filename) {
  if (length(sectionHeaders) < 1) stop("No section headers provided.")
  
  #multiple sections
  for (header in 1:length(sectionHeaders)) {
    #a blank section header indicates to match anywhere in the textToParse
    if (sectionHeaders[header] == "") sectionText <- textToParse
    
		#could be pretty inefficient if the same section header is repeated several times.
		#could build a list with divided output and check whether a section is present in the list before extracting
		else sectionText <- getMultilineSection(sectionHeaders[header], textToParse, filename)
    
    #process all fields for this section
    sectionFieldDF <- sectionFields[[header]]
    #browser()
    for (i in 1:nrow(sectionFieldDF)) {
      thisField <- sectionFieldDF[i,] 
      arglist[[ thisField$varName ]] <- extractValue(pattern=thisField$regexPattern, sectionText, filename, type=thisField$varType)
    }
  }

  return(arglist)
  
}


extractSummaries_1section <- function(modelFitSection, arglist, filename) {
	#function to extract model fit statistics from a section
	#wrapped to allow for multiple fit sections, as in EFA files.
	
    #MI and Montecarlo data types have fundamentally different output (means and sds per fit stat)
	if (grepl("imputation", arglist$DataType, ignore.case=TRUE) || grepl("montecarlo", arglist$DataType, ignore.case=TRUE)) {
		modelFitSectionHeaders <- c(
				"", #section-inspecific parameters
				"Chi-Square Test of Model Fit",
#        "Chi-Square Test of Model Fit for the Baseline Model",
				"Loglikelihood::H0 Value",
				"Loglikelihood::H1 Value",
				"CFI/TLI::CFI",
				"CFI/TLI::TLI",
				"Information Criteria::Akaike \\(AIC\\)",
				"Information Criteria::Bayesian \\(BIC\\)",
				"Information Criteria::Sample-Size Adjusted BIC \\(n\\* = \\(n \\+ 2\\) / 24\\)",
				"RMSEA \\(Root Mean Square Error Of Approximation\\)",        
				"WRMR \\(Weighted Root Mean Square Residual\\)"
		)
		modelFitSectionFields <- list(
				data.frame(
						varName=c("Parameters"), #defined outside of information criteria section for non-ML estimators
						regexPattern=c("Number of Free Parameters"),
						varType=c("int"), stringsAsFactors=FALSE
				),
				data.frame(
						varName=c("ChiSqM_DF", "ChiSqM_Mean", "ChiSqM_SD", "ChiSqM_NumComputations"), 
						regexPattern=c("Degrees of Freedom", "Mean", "Std Dev", "Number of successful computations"), 
						varType=c("int", "dec", "dec", "int"), stringsAsFactors=FALSE
				),
#        data.frame(
#            varName=c("ChiSqBaseline_Value", "ChiSqBaseline_DF", "ChiSqBaseline_PValue"), 
#            regexPattern=c("Value", "Degrees of Freedom", "^P-Value"), 
#            varType=c("dec", "int", "dec"), stringsAsFactors=FALSE
#        ),
				data.frame(
						varName=c("LL_Mean", "LL_SD", "LL_NumComputations"), 
						regexPattern=c("Mean", "Std Dev", "Number of successful computations"), 
						varType=c("dec", "dec", "int"), stringsAsFactors=FALSE
				),
				data.frame(
						varName=c("UnrestrictedLL_Mean", "UnrestrictedLL_SD", "UnrestrictedLL_NumComputations"), 
						regexPattern=c("Mean", "Std Dev", "Number of successful computations"), 
						varType=c("dec", "dec", "int"), stringsAsFactors=FALSE
				),
				data.frame(
						varName=c("CFI_Mean", "CFI_SD", "CFI_NumComputations"),
						regexPattern=c("Mean", "Std Dev", "Number of successful computations"),
						varType=c("dec", "dec", "int"), stringsAsFactors=FALSE
				),
				data.frame(
						varName=c("TLI_Mean", "TLI_SD", "TLI_NumComputations"),
						regexPattern=c("Mean", "Std Dev", "Number of successful computations"),
						varType=c("dec", "dec", "int"), stringsAsFactors=FALSE
				),
				data.frame(
						varName=c("AIC_Mean", "AIC_SD", "AIC_NumComputations"),
						regexPattern=c("Mean", "Std Dev", "Number of successful computations"),
						varType=c("dec", "dec", "int"), stringsAsFactors=FALSE
				),
				data.frame(
						varName=c("BIC_Mean", "BIC_SD", "BIC_NumComputations"),
						regexPattern=c("Mean", "Std Dev", "Number of successful computations"),
						varType=c("dec", "dec", "int"), stringsAsFactors=FALSE
				),
				data.frame(
						varName=c("aBIC_Mean", "aBIC_SD", "aBIC_NumComputations"),
						regexPattern=c("Mean", "Std Dev", "Number of successful computations"),
						varType=c("dec", "dec", "int"), stringsAsFactors=FALSE
				),
				data.frame(
						varName=c("RMSEA_Mean", "RMSEA_SD", "RMSEA_NumComputations"),
						regexPattern=c("Mean", "Std Dev", "Number of successful computations"),
						varType=c("dec", "dec", "int"), stringsAsFactors=FALSE
				),
				data.frame(
						varName=c("WRMR_Mean", "WRMR_SD", "WRMR_NumComputations"),
						regexPattern=c("Mean", "Std Dev", "Number of successful computations"),
						varType=c("dec", "dec", "int"), stringsAsFactors=FALSE
				)			
		)
		
		#handle two-level models, which return separate srmr for between vs. within
		if (grepl("twolevel", arglist$AnalysisType, ignore.case=TRUE)) {
			modelFitSectionHeaders <- append(modelFitSectionHeaders, c(
							"SRMR \\(Standardized Root Mean Square Residual\\) for the WITHIN level",
							"SRMR \\(Standardized Root Mean Square Residual\\) for the BETWEEN level"))
			
			modelFitSectionFields <- c(modelFitSectionFields,
					list(data.frame(
									varName=c("SRMR.Within_Mean", "SRMR.Within_SD", "SRMR.Within_NumComputations"),
									regexPattern=c("Mean", "Std Dev", "Number of successful computations"),
									varType=c("dec", "dec", "int"), stringsAsFactors=FALSE
							),
							data.frame(
									varName=c("SRMR.Between_Mean", "SRMR.Between_SD", "SRMR.Between_NumComputations"),
									regexPattern=c("Mean", "Std Dev", "Number of successful computations"),
									varType=c("dec", "dec", "int"), stringsAsFactors=FALSE
							))
			)
		}
		else {
			modelFitSectionHeaders <- append(modelFitSectionHeaders, "SRMR \\(Standardized Root Mean Square Residual\\)")
			modelFitSectionFields <- c(modelFitSectionFields, 
					list(data.frame(
									varName=c("SRMR_Mean", "SRMR_SD", "SRMR_NumComputations"),
									regexPattern=c("Mean", "Std Dev", "Number of successful computations"),
									varType=c("dec", "dec", "int"), stringsAsFactors=FALSE
							))
			)      
			
		}
		
	}
	else {
		modelFitSectionHeaders <- c(
				"", #section-inspecific parameters
				"Chi-Square Test of Model Fit",
				"Chi-Square Test of Model Fit for the Baseline Model",
				"Loglikelihood",
				"CFI/TLI",
				"Information Criteria",
				"RMSEA \\(Root Mean Square Error Of Approximation\\)",
				"WRMR \\(Weighted Root Mean Square Residual\\)"
		)
		modelFitSectionFields <- list(
				data.frame(
						varName=c("Parameters"), #defined outside of information criteria section for non-ML estimators
						regexPattern=c("Number of Free Parameters"),
						varType=c("int"), stringsAsFactors=FALSE
				),
				data.frame(
						varName=c("ChiSqM_Value", "ChiSqM_DF", "ChiSqM_PValue", "ChiSqM_ScalingCorrection"), 
						regexPattern=c("^\\s*Value", "Degrees of Freedom", "^\\s*P-Value", "Scaling Correction Factor"), 
						varType=c("dec", "int", "dec", "dec"), stringsAsFactors=FALSE
				),
				data.frame(
						varName=c("ChiSqBaseline_Value", "ChiSqBaseline_DF", "ChiSqBaseline_PValue"), 
						regexPattern=c("^\\s*Value", "Degrees of Freedom", "^\\s*P-Value"), 
						varType=c("dec", "int", "dec"), stringsAsFactors=FALSE
				),
				data.frame(
						varName=c("LL", "UnrestrictedLL", "LLCorrectionFactor", "UnrestrictedLLCorrectionFactor"), 
						regexPattern=c("H0 Value", "H1 Value", "H0 Scaling Correction Factor", "H1 Scaling Correction Factor"), 
						varType=c("dec", "dec", "dec", "dec"), stringsAsFactors=FALSE
				),
				data.frame(
						varName=c("CFI", "TLI"),
						regexPattern=c("CFI", "TLI"),
						varType=c("dec", "dec"), stringsAsFactors=FALSE
				),
				data.frame(
						varName=c("AIC", "BIC", "aBIC"),
						regexPattern=c("Akaike \\(AIC\\)", "Bayesian \\(BIC\\)", "Sample-Size Adjusted BIC"),
						varType=c("dec", "dec", "dec"), stringsAsFactors=FALSE
				),
				data.frame(
						varName=c("RMSEA_Estimate", "RMSEA_90CI_LB", "RMSEA_90CI_UB", "RMSEA_pLT05"),
						regexPattern=c("Estimate", "90 Percent C.I.", "90 Percent C.I.", "Probability RMSEA <= .05"),
						varType=c("dec", "dec[1]", "dec[2]", "dec"), stringsAsFactors=FALSE
				),
				data.frame(
						varName=c("WRMR"),
						regexPattern=c("Value"),
						varType=c("dec"), stringsAsFactors=FALSE
				)
		)
		
		if (grepl("twolevel", arglist$AnalysisType, ignore.case=TRUE)) {
			
			modelFitSectionHeaders <- append(modelFitSectionHeaders, "SRMR \\(Standardized Root Mean Square Residual\\)")
			
			modelFitSectionFields <- c(modelFitSectionFields, 
					list(data.frame(
									varName=c("SRMR.Within", "SRMR.Between"),
									regexPattern=c("Value for Within", "Value for Between"),
									varType=c("dec", "dec"), stringsAsFactors=FALSE
							))
			)
			
		}
		else {
			
			modelFitSectionHeaders <- append(modelFitSectionHeaders, "SRMR \\(Standardized Root Mean Square Residual\\)")
			
			#append two lists together
			modelFitSectionFields <- c(modelFitSectionFields, 
					list(data.frame(
									varName=c("SRMR"),
									regexPattern=c("Value"),
									varType=c("dec"), stringsAsFactors=FALSE
							)
					))
		}
	}
	
	arglist <- extractSummaries_1plan(arglist, modelFitSectionHeaders, modelFitSectionFields, modelFitSection, filename)
	return(arglist)
}

extractSummaries_1file <- function(outfiletext, filename, extract=c("Title", "LL", "BIC", "AIC", "AICC",
  "Parameters", "Observations", "BLRT", "RMSEA", "CFI", "TLI", "ChiSqModel", "aBIC", 
  "Estimator", "SRMR", "WRMR", "ChiSqBaseline"))
{
#extractSummaries_1file(outfiletext, filename)
#   outfiletext: this is the output file in string form to be parsed. Passed in from extractModelSummaries.
#   filename: name of the file being parsed. Used in case of bad model, prints a warning.
#
#   Description: This function parses an output file for specific model details.
#   It returns a list of model details for a single output file.

  #preallocates list
  #arglist = vector("list", length(extract))  
  arglist <- list()
    
  #PROCESS FIELDS OF INTEREST IN INPUT INSTRUCTIONS SECTION
  startInput <- grep("^\\s*INPUT INSTRUCTIONS\\s*$", outfiletext, ignore.case=TRUE, perl=TRUE)
  if (length(startInput) == 0) warning("Could not find beginning of input")
     
  endInput <- grep("^\\s*(INPUT READING TERMINATED NORMALLY|\\d+ WARNING\\(S\\) FOUND IN THE INPUT INSTRUCTIONS)\\s*$", outfiletext, ignore.case=TRUE, perl=TRUE)
  if (length(endInput) == 0) warning("Could not find end of input")

  inputSection <- outfiletext[(startInput+1):(endInput-1)]
  inputHeaders <- grep("^\\s*(data:|variable:|define:|analysis:|model:|output:|savedata:|plot:|montecarlo:)", inputSection, ignore.case=TRUE, perl=TRUE)
  
  #PROCESS TITLE
  if ("Title" %in% extract) {
    titleStart <- grep("^\\s*title:", inputSection, ignore.case=TRUE, perl=TRUE)
    if (length(titleStart) == 1) {
      
      titleEnd <- inputHeaders[inputHeaders > titleStart][1] - 1 #subtract 1 to go to line preceding next keyword
      stopifnot(titleEnd > 0)
      
      #if title spans multiple lines, then collapse into one string
      title <- paste(inputSection[titleStart:titleEnd], collapse=" ")
      
      #delete the "Title: " piece from the match       
      title <- sub("^\\s*title:\\s+", "", title, ignore.case=TRUE, perl=TRUE)
      
      #convert multiple spaces into a single space (occurs when title spans many lines and has indentation)
      title <- gsub("\\s+", " ", title, ignore.case=TRUE, perl=TRUE)
      
      arglist$Title <- title
    }
    else {
      warning("Unable to locate title field. Returning missing")
      arglist$Title <- NA_character_
    }
  }
  
  #extract the analysis type, which is important for setting other parameters.
	#allow for analysis type to reside on same line as analysis section header
  analysisSectionStart <- grep("^\\s*analysis\\s*:.*$", inputSection, ignore.case=TRUE, perl=TRUE)
  if (length(analysisSectionStart) > 0) {
    analysisSectionEnd <- inputHeaders[inputHeaders > analysisSectionStart][1] - 1
    analysisSection <- inputSection[analysisSectionStart:analysisSectionEnd]
    
    analysisTypeLine <- grep("^(\\s*analysis:)*\\s*TYPE\\s*(IS|=|ARE)\\s*.*;$", analysisSection, ignore.case=TRUE, perl=TRUE, value=TRUE) 
    
    if (length(analysisTypeLine) > 0)
      arglist$AnalysisType <- sub("^(\\s*analysis:)*\\s*TYPE\\s*(IS|=|ARE)\\s*(.*);", "\\3", analysisTypeLine, ignore.case=TRUE, perl=TRUE)
    else
      arglist$AnalysisType <- "GENERAL"
  }
  else arglist$AnalysisType <- "GENERAL" #no analysis section specified in input. Default general
  
	#extract the data type (important for detecting imputation datasets)
	dataSectionStart <- grep("^\\s*data\\s*:.*$", inputSection, ignore.case=TRUE, perl=TRUE)
	if (length(dataSectionStart) > 0) {
		
		dataSectionEnd <- inputHeaders[inputHeaders > dataSectionStart][1] - 1
		dataSection <- inputSection[dataSectionStart:dataSectionEnd]
		
		dataTypeLine <- grep("^(\\s*data:)*\\s*TYPE\\s*(IS|=|ARE)\\s*.*;$", dataSection, ignore.case=TRUE, perl=TRUE, value=TRUE) 
		
		if (length(dataTypeLine) > 0)
			arglist$DataType <- sub("^(\\s*data:)*\\s*TYPE\\s*(IS|=|ARE)\\s*(.*);", "\\3", dataTypeLine, ignore.case=TRUE, perl=TRUE)
		else
			arglist$DataType <- "INDIVIDUAL"
	}
	else arglist$DataType <- "INDIVIDUAL" #no data section specified in input. Default individual
	#END INPUT INSTRUCTIONS PROCESSING  

  #BEGIN ANALYSIS SUMMARY PROCESSING
  analysisSummarySection <- getMajorSection("^\\s*SUMMARY OF ANALYSIS\\s*$", outfiletext)

  if ("Estimator" %in% extract)
    arglist$Estimator <- extractValue(pattern="^\\s*Estimator\\s*", analysisSummarySection, filename, type="str")

  if ("Observations" %in% extract)
    arglist$Observations <- extractValue(pattern="^\\s*Number of observations\\s*", analysisSummarySection, filename, type="int")
  
  
  #END ANALYSIS SUMMARY PROCESSING

  #BEGIN MODEL FIT STATISTICS PROCESSING
	#handle EFA output, which has separate model fit sections within each file
	#do this by extracting model fit sections for each and using an rbind call

  if (grepl("(?!MIXTURE|TWOLEVEL)\\s*EFA\\s+", arglist$AnalysisType, ignore.case=TRUE, perl=TRUE)) {

		factorLB <- as.numeric(sub(".*EFA\\s+(\\d+).*", "\\1", arglist$AnalysisType, perl=TRUE))
		factorUB <- as.numeric(sub(".*EFA\\s+\\d+\\s+(\\d+).*", "\\1", arglist$AnalysisType, perl=TRUE))
		factorSeq <- seq(factorLB, factorUB)
		EFASections <- grep(paste("^\\s*EXPLORATORY FACTOR ANALYSIS WITH (", 
						paste(factorSeq, collapse="|"), ") FACTOR\\(S\\):\\s*$", sep=""), outfiletext, perl=TRUE) 

		if (!length(EFASections) > 0) stop("Unable to locate section headers for EFA model fit statistics")
		
    #need to convert from list to data.frame format to allow for proper handling of rbind below
    arglistBase <- as.data.frame(arglist, stringsAsFactors=FALSE)
    
    efaList <- list()
		for (thisFactor in 1:length(factorSeq)) {
			#subset output by starting text to be searched at the point where factor output begins
      modelFitSection <- getMajorSection("^(TESTS OF MODEL FIT|MODEL FIT INFORMATION)$", outfiletext[EFASections[thisFactor]:length(outfiletext)])
      
      efaList[[thisFactor]] <- extractSummaries_1section(modelFitSection, arglistBase, filename)
      efaList[[thisFactor]]$NumFactors <- factorSeq[thisFactor]
		}

    arglist <- do.call(rbind, efaList)
	}
	else {

		modelFitSection <- getMajorSection("^(TESTS OF MODEL FIT|MODEL FIT INFORMATION)$", outfiletext)
    arglist <- extractSummaries_1section(modelFitSection, arglist, filename)
	}

  #CLASSIFICATION QUALITY  
  classificationQuality <- getMajorSection("^CLASSIFICATION QUALITY$", outfiletext)
  
  if (!is.null(classificationQuality))
    arglist$Entropy <- extractValue(pattern="^\\s*Entropy\\s*", classificationQuality, filename, type="dec")
    #overkill
    #arglist <- extractSummaries_1plan(arglist, "", list(data.frame(varName="Entropy", regexPattern="Entropy", varType=c("dec"), stringsAsFactors=FALSE)), classificationQuality, filename)
  else
    arglist$Entropy <- NA_real_ #maybe try to avoid the is null logic and just have extractModelSummary correctly handle null sections

  
  #the BLRT keyword serves as a placeholder for extracting many fields, like km1likelihood
  if ("BLRT" %in% extract) {
    
    #locate the beginning of the BLRT section
    matchlines <- grep("TECHNICAL 14 OUTPUT", outfiletext)
    
    if (length(matchlines) == 1) {
      #match up through the end of the file
      endRange <- grep("3463 Stoner Ave\\.", outfiletext)
      
      if (!length(endRange) == 1) {
        stop("Problem identifying end marker for BLRT")
      }
      
      blrtpiece <- outfiletext[matchlines:endRange]
      
      arglist$BLRT_KM1LL <- extractValue(pattern="H0 Loglikelihood Value", blrtpiece, filename, type="dec")
      arglist$BLRT_PValue <- extractValue(pattern="Approximate P-Value", blrtpiece, filename, type="dec")
      arglist$BLRT_Numdraws <- extractValue(pattern="Successful Bootstrap Draws", blrtpiece, filename, type="int")
    }
    else {
      #warning("Could not locate BLRT section, despite being requested")
      
      #need to pad the expected fields with NAs to keep the list length consistent, permitting correct rbind
      arglist$BLRT_KM1LL <- as.numeric(NA)
      arglist$BLRT_PValue <- as.numeric(NA)
      arglist$BLRT_Numdraws <- as.numeric(NA)
    }
  }

  
	#calculate adjusted AIC per Burnham & Anderson(2004), which is better than AIC for non-nested model selection
	#handle AICC calculation, requires AIC, Parameters, and observations
  if (all(c("AICC", "AIC", "Parameters", "Observations") %in% extract)) {
		if (!is.null(arglist$Parameters) && !is.na(arglist$Parameters) &&
				!is.null(arglist$AIC) && !is.na(arglist$AIC) &&
				!is.null(arglist$Observations) && !is.na(arglist$Observations))
			arglist$AICC <- arglist$AIC + (2*arglist$Parameters*(arglist$Parameters+1))/(arglist$Observations-arglist$Parameters-1)
		else
			arglist$AICC <- NA_real_
  }
	
  
    
  #Only warn about missing LL for ML-based estimators
#too convoluted to maintain (and not so useful), generating errors I don't want to debug
#  if ("Estimator" %in% extract && "LL" %in% extract 
#			&& !is.na(arglist$Estimator) && arglist$Estimator %in% c("ML", "MLR", "MLM", "MLMV", "MLF") 
#			&& ((grepl("imputation", arglist$DataType, ignore.case=TRUE) && is.na(arglist$LL_Mean))
#			|| (!grepl("imputation", arglist$DataType, ignore.case=TRUE) && is.na(arglist$LL))))			
#    warning("Model missing LL value, despite use of ML-based estimator. Likely a failed run.\n  ", filename)
#  
  
  #for now, skip including input instructions in the returned data.frame. Makes the output too cluttered.
  #arglist$InputInstructions <- paste((outfiletext[(startInput+1):(endInput-1)]), collapse="\n")
  arglist$Filename <- splitFilePath(filename)$filename #only retain filename, not path

  return(as.data.frame(arglist, stringsAsFactors=FALSE))
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
  
	require(plyr)
	
	#retain working directory and reset at end of run
  curdir <- getwd()
  
	outfiles <- getOutFileList(target, recursive, filefilter)
	
  details <- c()
  
  #for each output file, use the extractSummaries_1file function to extract relevant data
  #note that extractSummaries_1file returns data as a list
  #rbind creates an array of lists by appending each extractSummaries_1file return value
  for (i in 1:length(outfiles)) {
    #read the file
    readfile <- scan(outfiles[i], what="character", sep="\n", strip.white=FALSE, blank.lines.skip=FALSE)
    
    #bomb out for EFA files
    if (length(grep("TYPE\\s+(IS|=|ARE)\\s+((MIXTURE|TWOLEVEL)\\s+)+EFA\\s+\\d+", readfile, ignore.case=TRUE, perl=TRUE)) > 0) {
      warning(paste("EFA, MIXTURE EFA, and TWOLEVEL EFA files are not currently supported by extractModelSummaries.\n  Skipping outfile: ", outfiles[i], sep=""))
      next #skip file
    }
    
    #append params for this file to the details array
    #note that this is a memory-inefficient solution because of repeated copying. Better to pre-allocate.
		details <- rbind.fill(details, extractSummaries_1file(readfile, outfiles[i]))
  }
  
  #reset working directory
  setwd(curdir)

	#cleanup columns containing only NAs
	for (col in names(details)) {
		if (all(is.na(details[[col]]))) details[[col]] <- NULL
	}
		
	return(details)
}

getSavedata_Data <- function(outfile) {
  #exposed wrapper for l_getSavedata_Data, which pulls saveData data into data.frame
	if(!file.exists(outfile)) {
		stop("Cannot locate outfile: ", outfile)
	}
		
  outfiletext <- scan(outfile, what="character", sep="\n", strip.white=FALSE)
  
  if (length(outfiletext) == 0) {
    warning("Empty outfile")
    return(NULL)
  }
  
  return(l_getSavedata_Data(outfile, outfiletext))
}

l_getSavedata_Data <- function(outfile, outfiletext) {

  #outfile should be a relative or absolute path to the .out file with the savedata section to be parsed
  #if no directory is provided, the file is assumed with be within the working directory getwd().

  outfileDirectory <- splitFilePath(outfile)$directory
  fileInfo <- l_getSavedata_Fileinfo(outfile, outfiletext)

  #if fileinfo could not be loaded (no savedata section), then abort data pull
  if (is.null(fileInfo)) return(NULL)

  savedataSplit <- splitFilePath(fileInfo$fileName)
  
  #if outfile target directory is non-empty, but savedataFile is without directory, then append
  #outfile directory to savedataFile. This ensures that R need not be in the working directory
  #to read the savedataFile. But if savedataFile has an absolute directory, don't append

  #if savedata directory is present and absolute, or if no directory in outfile, just use filename as is
  if (!is.na(savedataSplit$directory) && savedataSplit$absolute)
    savedataFile <- fileInfo$fileName #just use savedata filename if has absolute path
  else if (is.na(outfileDirectory))
    savedataFile <- fileInfo$fileName #just use savedata filename if outfile is missing path (working dir)
  else
    savedataFile <- file.path(outfileDirectory, fileInfo$fileName) #savedata path relative or absent and outfile dir is present

  #cat("Outfile dir: ", outfileDirectory, "\n")
  #cat("Savedata directory: ", savedataSplit$directory, "\n")
  #cat("concat result: ", savedataFile, "\n")


  #need to read as fixed width format given the way Mplus left-aligns missing vals (*)
  #dataset <- read.table(file=file.path(path, fileInfo$fileName), header=FALSE, 
  #    na.strings="*", col.names=fileInfo$varNames)

  #strip.white is necessary for na.strings to work effectively with fixed width fields
  #otherwise would need something like "*       " for na.strings
  dataset <- read.fwf(file=savedataFile, widths=fileInfo$varWidths, header=FALSE,
      na.strings="*", col.names=fileInfo$varNames, strip.white=TRUE)
  
  return(dataset)
}

addHeaderToSavedata <- function(outfile, directory=getwd()) {
  
}

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
  
  outfiletext <- scan(outfile, what="character", sep="\n", strip.white=FALSE)
  
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
  savedataStart <- grep("^\\s*SAVEDATA INFORMATION\\s*$", outfiletext, ignore.case=TRUE, perl=TRUE)
  savedataEnd <- grep("^\\s*Save file record length\\s+\\d+$", outfiletext, ignore.case=TRUE, perl=TRUE)
  
  
  #need to have beginning and end
  if (! (length(savedataStart) > 0 && length(savedataEnd) > 0)) {
    #omit warning -- probably more common for section to be missing
		#warning("Unable to locate a SAVEDATA section in output file: ", outfile)
    return(NULL)
  }
  
  savedataSection <- outfiletext[savedataStart:savedataEnd]

  orderStart <- grep("^\\s*Order and format of variables\\s*$", savedataSection, ignore.case=TRUE, perl=TRUE)
  if (!length(orderStart) > 0) {
    #save data section exists, but doesn't contain this output. Maybe other savedata stuff, like bayesian, tech4, etc.
    return(NULL)
  }
  
  saveFileStart <- grep("^\\s*Save file\\s*$", savedataSection, ignore.case=TRUE, perl=TRUE)

  #dump any blank fields because they will cause nulls in the names, formats, widths.
  #This is handled by blank.lines.skip=TRUE in wrappers, but readModels needs to retain blank lines
  #for other functions, so strip here.
  variablesToParse <- savedataSection[(orderStart+1):(saveFileStart-1)]
  variablesToParse <- variablesToParse[variablesToParse != ""]
  
  variableNames <- sub("^\\s*([\\w\\d\\.]+)\\s+[\\w\\d\\.]+\\s*$", "\\1", variablesToParse, perl=TRUE)
  variableFormats <- sub("^\\s*[\\w\\d\\.]+\\s+([\\w\\d\\.]+)\\s*$", "\\1", variablesToParse, perl=TRUE)
  
  variableWidths <- strapply(variableFormats, "[IEFG]+(\\d+)(\\.\\d+)*", as.numeric, perl=TRUE, simplify=TRUE)
  
  #trim leading and trailing space from the filename
  fileName <- sub("^\\s*","", savedataSection[saveFileStart+1], perl=TRUE)
  fileName <- sub("\\s*$","", fileName, perl=TRUE)
  
  #return the file information as a list
  return(list(fileName=fileName, varNames=variableNames, varFormats=variableFormats, varWidths=variableWidths))
}


#a helper function to be used by wrappers that generate HTML, LaTex, and on-screen displays of summary statistics
subsetModelList <- function(modelList, keepCols, dropCols, sortBy) {
  #only allow keep OR drop.
  if(!missing(keepCols) && !missing(dropCols)) stop("keepCols and dropCols passed to subsetModelList. You must choose one or the other, but not both.")
    
  #if did not pass either drop or keep, setup useful defaults
  if (missing(keepCols) && missing(dropCols)) keepCols <- c("Title", "LL", "Parameters", "AIC", "AICC", "BIC", "RMSEA_Estimate")
    
  #keep only columns specified by keepCols
  if (!missing(keepCols) && length(keepCols) > 0) {
    #check to make sure each column exists if keepCols used    
    summaryNames <- names(modelList)
    for (colName in keepCols) {
      if (!colName %in% summaryNames) keepCols <- keepCols[-which(keepCols==colName)]
    }
    
    if (length(keepCols) == 0) stop("All fields passed as keepCols are missing from data.frame\n  Fields in data.frame are:\n  ", paste(strwrap(paste(summaryNames, collapse=" ", sep=""), width=80, exdent=4), collapse="\n"))
    MplusData <- modelList[, keepCols, drop=FALSE]
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

  #handle cases where sortBy is missing 
  if (missing(sortBy)) {
    if ("AICC" %in% notMissing) sortBy <- "AICC"
    else if ("AIC" %in% notMissing) sortBy <- "AIC"
    else if ("BIC" %in% notMissing) sortBy <- "BIC"
    else if ("Title" %in% notMissing) sortBy <- "Title"
    else sortBy <- NA_character_
  }

  if (!sortBy %in% notMissing) stop("sortBy field: ", sortBy, " is not present in the summary data.frame.\n  Check your keepCols and dropCols arguments and the summary data.frame")
  
  #sort data set correctly and drop columns where all models are missing
  #need drop=FALSE to retain as data.frame in case only one column returned
  MplusData <- MplusData[order(MplusData[[sortBy]]), notMissing, drop=FALSE]
  
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