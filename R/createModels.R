createModels <- function(templatefile, run=F) {
  if (!file.exists(templatefile)) stop("Template file not found.")
  #extract init section
  readfile <- scan(templatefile, what="character", sep="\n", strip.white=FALSE)
  
  #divide into init versus body
  startinit <- grep("[[init]]", readfile, fixed=T)
  endinit <- grep("[[/init]]", readfile, fixed=T)
  
  if (length(startinit) != 1 || length(endinit) != 1) {
    stop("Unable to find init section in template file.")
  }
  
  initsection <- readfile[(startinit+1):(endinit-1)]
  bodysection <- readfile[(endinit+1):length(readfile)]
  
  toProcess <- processInit(initsection)
  
  #identify all lines that contain a tag
  toProcess$.allTags <- grep("\\[\\[\\s*[\\w=><!#]+\\s*\\]\\]", bodysection, perl=T)
  
  #match all tags that consist only of word chars (no #, =, <, etc.) and that are not the iterators
  #this uses a zero-length lookahead assertion prior to the word matching to exclude iterators
  regexp <- paste("\\[\\[\\s*(?!", paste(toProcess$iterators, collapse="|"), ")\\w+\\s*\\]\\]", sep="")
  toProcess$.simpleTags <- grep(regexp, bodysection[toProcess$.allTags], perl=T)
  bodysection[toProcess$.allTags][toProcess$.simpleTags] <- replaceTags(regexp, bodysection[toProcess$.allTags][toProcess$.simpleTags], toProcess, replaceType="simple")
  
  #kick off the recursive replace
  if (length(toProcess$iterators) > 0) {
    recurseReplace(bodysection, toProcess)
  }
}

replaceTags <- function(regexp=NULL, syntaxFile, toProcess, perl=T, replaceType) {
  if (!is.list(toProcess)) {
    stop("Argument list passed to replaceTags is not a list")
  }
  
  #there's a chance that the syntax passed in was empty (if the data were subset outside of the function)
  if (length(syntaxFile) == 0) {
    return(NA)
  }
  
  if (replaceType == "simple" || replaceType == "final") {
    regexp <- paste("\\[\\[\\s*(?!", paste(toProcess$iterators, collapse="|"), ")(\\w+)\\s*\\]\\]", sep="")
    tags <- gsubfn(regexp, function(tag, simpleWord) {
          replaceText <- eval(parse(text=paste("toProcess$", simpleWord, sep="")))
          if (is.null(replaceText)) {
            stop("When replacing tag: ", tag, ", could not find corresponding value.")
          }
          replaceText
        }, syntaxFile, perl=perl, backref=1)
  }
  else if (replaceType == "array") {
    regexp <- paste("\\[\\[\\s*(\\w+)#", toProcess$curIteratorName, "\\s*\\]\\]", sep="")
    tags <- gsubfn(regexp, function(tag, arrayMatch) {
          
          replaceText <- eval(parse(text=paste("toProcess$", arrayMatch, "[", toProcess$curItPos[toProcess$curIteratorDepth], "]", sep="")))
          if (is.null(replaceText)) {
            stop("When replacing tag: ", tag, ", could not find corresponding value.")
          }
          replaceText
        }, syntaxFile, perl=perl, backref=1)
  }
  else if (replaceType == "iterator") {
    regexp <- paste("\\[\\[\\s*(", toProcess$curIteratorName, ")\\s*\\]\\]", sep="")
    tags <- gsubfn(regexp, function(tag, arrayMatch) {
          #it only makes sense to do replacements at the bottom level
          
          replaceText <- as.character(toProcess$curItPos[toProcess$curIteratorDepth])
          if (is.null(replaceText)) {
            stop("When replacing tag: ", tag, ", could not find corresponding value.")
          }
          replaceText
        }, syntaxFile, perl=perl, backref=1)
  }
  
  if (replaceType == "final") {
    #replace simple iterator statements
    
    regexp <- paste("\\[\\[\\s*(", paste(toProcess$iterators, collapse="|"), ")\\s*\\]\\]", sep="")
    
    #replace simple iterators
    tags <- gsubfn(regexp, function(tag, arrayMatch) {
          #because could be any iterator, need to find the position of the matched iterator within the vector
          #this allows proper reference to the current loop position for the iterator of interest
          iteratorPosition <- grep(paste("\\b", arrayMatch, "\\b", sep=""), toProcess$iterators, perl=T)
          
          #find current position for this iterator and replace [[n]] values with iterator value
          if (length(iteratorPosition) == 1) {
            replaceText <- as.character(toProcess$curItPos[iteratorPosition])
          }
          
          if (is.null(replaceText)) {
            stop("When replacing tag: ", tag, ", could not find corresponding value.")
          }
          replaceText
        }, tags, perl=perl, backref=1) #carry forward tags from above, simple replace
    
    #replace array tags        
    regexp <- paste("\\[\\[\\s*\\b(\\w+)#(", paste(toProcess$iterators, collapse="|"), ")\\b\\s*\\]\\]", sep="")
    tags <- gsubfn(regexp, function(tag, arrayMatch, iteratorMatch) {
          iteratorPosition <- grep(paste("\\b", iteratorMatch, "\\b", sep=""), toProcess$iterators, perl=T)
          
          if (length(iteratorPosition) == 1) {
            replaceText <- eval(parse(text=paste("toProcess$", arrayMatch, "[", toProcess$curItPos[iteratorPosition], "]", sep="")))
          }
          
          if (is.null(replaceText)) {
            stop("When replacing tag: ", tag, ", could not find corresponding value.")
          }
          replaceText
          
        }, tags, perl=perl, backref=2) #carry forward tags from above simple iterators
    
  }
  tags
}

evaluateConditional <- function(tag, toProcess) {
  #evaluate whether tag is true
  #first divide up into name, operator, and value
  regexp <- "(\\w+)\\s*([!><=]+)\\s*(\\d+)"
  conditional <- unlist(strapply(tag, regexp, c))
  
  if (length(conditional) < 3) {
    stop("Error in conditional tag: does not contain variable, operator, and value. Tag = ", tag)
  }
  
  #convert simple equals to logical equals
  if (conditional[2] == "=") conditional[2] = "=="
  
  iteratorPosition <- grep(paste("\\b", conditional[1], "\\b", sep=""), toProcess$iterators, perl=T)
  
  #return a boolean value indicating whether the conditional is true
  return(eval(parse(text=paste("toProcess$curItPos[iteratorPosition]", conditional[2], conditional[3], sep=""))))
  
}

#Because it handles code blocks, this will require the entire file, not just select tags
processConditionalTags <- function(syntaxFile, toProcess, perl=T) {
  #handle conditional tags
  #note that the first parenthesis matches the entire tag (e.g., "m=4"), but the second parenthesis matches one of the iterators
  #thus, \1 is the entire tag and \2 is just the iterator matched
  #scratch above... use a lookahead to test for iterator, but don't match it. So now only \\1 exists and it matches whole tag
  #use strapply to return vector of matches
  regexp <- paste("\\[\\[\\s*(?:", paste(toProcess$iterators, collapse="|"), ")\\s*[!><=]+\\s*\\d+\\s*\\]\\]", sep="")
  
  #locate line positions of conditional start tags
  tagStarts <- grep(regexp, syntaxFile, perl=T)
  
  if (length(tagStarts) == 0) {
    #no matches
    return(syntaxFile)
  }
  
  linesToDrop <- c()
  for (i in 1:length(tagStarts)) {
    #grab just the conditional tag
    regexp <- paste("\\[\\[\\s*((?:", paste(toProcess$iterators, collapse="|"), ")\\s*[!><=]+\\s*\\d+)\\s*\\]\\]", sep="")
    #if there is more than one matching tag on this line, it will return a vector
    #need to pass backref=-1 so that it does not create an empty value for the lookahead operator
    #the function defaults to -2 because of the lookahead, which results in an erroneous empty
    #the empty breaks the search for a closing tag. see ?gsubfn
    tagPart <- unlist(strapply(syntaxFile[tagStarts][i], regexp, c, perl=T, backref=-1))
    
    #NOTE: the way this is written that does not permit combining same-line conditionals with multi-line matches
    for (j in 1:length(tagPart)) {
      #find end tag by adding / to the tag
      endregexp <- paste("\\[\\[\\s*\\/", tagPart[j], "\\s*\\]\\]", sep="")
      
      #check for the ending tag on the current line
      sameLineEnd <- regexpr(endregexp, syntaxFile[tagStarts][i], perl=T)
      
      if (sameLineEnd > 0) {
        #okay, the closing tag is on the same line, so we want to locate the positions of the opening tag
        startPos <- regexpr(regexp, syntaxFile[tagStarts][i], perl=TRUE)
        
        #if the conditional is true, remove only the tags. If false, remove the entire piece of the line between tags
        if (evaluateConditional(tagPart[j], toProcess)) {
          #extract only the tags, leaving the text
          syntaxFile[tagStarts][i] <- paste(substring(syntaxFile[tagStarts][i], c(1, (startPos + attr(startPos, "match.length")), (sameLineEnd + attr(sameLineEnd, "match.length"))), c((startPos - 1), (sameLineEnd - 1), nchar(syntaxFile[tagStarts][i]))), collapse="")
        }
        else {
          #extract the full length between and including tags
          syntaxFile[tagStarts][i] <- paste(substring(syntaxFile[tagStarts][i], c(1, (sameLineEnd + attr(sameLineEnd, "match.length"))), c((startPos - 1), nchar(syntaxFile[tagStarts][i]))), collapse="")
        }
      }
      else { #closing tag not on same line, use the delete lines logic
        #subsets syntax file for only rows after the tag start    
        tagEnd <- grep(endregexp, syntaxFile[-1:-tagStarts[i]], perl=T)
        if (length(tagEnd) >= 1) {
          #only keep the nearest match (could lead to problems if same tag is nested, but can't envision how that would be needed)
          #express tag end in terms of its position after tagStarts (since data subset above)
          tagEnd <- tagEnd[1] + tagStarts[i]
        }
        else if (length(tagEnd) == 0) {
          stop("Could not find matching close tag for conditional tag: ", tagPart)
        }
        
        if (evaluateConditional(tagPart[j], toProcess)) {
          #if the conditional is true, just remove the start and stop lines from the syntax file
          linesToDrop <- c(linesToDrop, tagStarts[i], tagEnd)
        }
        else {
          #remove all lines between start and stop
          linesToDrop <- c(linesToDrop, tagStarts[i]:tagEnd)
        }
        
      }
    }
  }
  
  #drop lines based on logic above
  syntaxFile <- syntaxFile[!1:length(syntaxFile) %in% linesToDrop]
  
  return(syntaxFile)
}

processForeachTags <- function(syntaxFile, toProcess) {
  #search for a foreach tag with this iterator
  
  regexp <- paste("\\[\\[\\s*foreach\\s+", toProcess$curIteratorName, "\\s*\\]\\]", sep="")  
  
  #find all positions related to this iterator
  tagStarts <- grep(regexp, syntaxFile, perl=T)
  
  if (length(tagStarts) == 0) {
    #nothing to see here
    return(syntaxFile)
  }
  
  for (i in 1:length(tagStarts)) {
    #find first foreach closing tag
    endregexp <- paste("\\[\\[\\s*\\/foreach\\s*\\]\\]", sep="")
    #subsets syntax file for only rows after the tag start
    
    tagEnd <- grep(endregexp, syntaxFile[-1:-tagStarts[i]], perl=T)
    if (length(tagEnd) >= 1) {
      #only keep the first match
      #express tag end in terms of its position after tagStarts (since data subset above)
      tagEnd <- tagEnd[1] + tagStarts[i]
    }
    else if (length(tagEnd) == 0) {
      stop("Could not find matching close tag for foreach tag for iterator: ", toProcess$curIteratorName)
    }
    
    #grab subsection between beginning and end
    workingSection <- syntaxFile[(tagStarts[i]+1):(tagEnd-1)]
    #this could be a problem  because processConditional handles all levels, but we want only this level (subscripting errors?)
    workingSection <- processConditionalTags(workingSection, toProcess, perl=T)
    workingSection <- replaceTags(regexp=NULL, workingSection, toProcess, perl=T, replaceType="array")
    workingSection <- replaceTags(regexp=NULL, workingSection, toProcess, perl=T, replaceType="iterator")
    
    #insert the result above the foreach tag
    
    #only leave in foreach section under 2 conditions:
    # 1) current loop position within this iterator is less than the max for this iterator
    # 2) not on the last iterator of the set, at which point the file is written (concerned this may lead to dropped foreach on subsequent runs)
    #PROBLEM WITH #2, DELETES FOREACH PREMATURELY
    if(toProcess$curItPos[toProcess$curIteratorDepth] < max(toProcess[[toProcess$curIteratorName]])) { # && toProcess$curIteratorDepth < length(toProcess$iterators)) {
      #paste workingSection above foreach section
      syntaxFile <- c(syntaxFile[1:(tagStarts[i]-1)], workingSection, syntaxFile[tagStarts[i]:length(syntaxFile)])
    }
    else {
      #insert working section prior to opening tag and only include the syntax after the tag closes, effectively
      #deleting the tagged foreach section
      syntaxFile <- c(syntaxFile[1:(tagStarts[i]-1)], workingSection, syntaxFile[(tagEnd+1):length(syntaxFile)])    
    }
    
    
    return(syntaxFile)
    
  }
}

recurseReplace <- function(bodysection, toProcess, curiterator=1) {
  
  if (!is.list(toProcess)) {
    stop("Argument list passed to recurseReplace is not a list")
  }
  
  thisIterator <- toProcess$iterators[curiterator]
  
  #set the current iterator for the collection (used by replaceTags)
  toProcess$curIteratorName <- thisIterator
  toProcess$curIteratorDepth <- curiterator
  
  for (i in toProcess[[thisIterator]]) {
    #replace all tags for this iterator and this iteration
    
    #set the current position within this iterator for use in replace tags
    #create a vector of iterator positions for use in replaceTags
    toProcess$curItPos[curiterator] <- i
    print (paste("current iterator is: ", thisIterator, " position:", as.character(i)))
    
    #process foreach commands
    bodysection <- processForeachTags(bodysection, toProcess)
    
    if (curiterator < length(toProcess$iterators)) {
      #recurse to the next level by adding 1 to the iterator
      #bodysection <- recurseReplace(bodysection, toProcess, curiterator = curiterator+1)
      recurseReplace(bodysection, toProcess, curiterator = curiterator+1)
    }
    else {
      #don't overwrite bodysection, which should retain its tags
      #process conditional tags first because it may alter the content of the tags to be parsed
      toWrite <- processConditionalTags(bodysection, toProcess, perl=T)
      toWrite <- replaceTags(regexp=NULL, toWrite, toProcess, perl=T, replaceType="final")
      
      #kludgy but necessary: clean up foreach tags
      regexp <- paste("\\[\\[\\s*foreach\\s+(", paste(toProcess$iterators, collapse="|"), ")\\s*\\]\\]", sep="")
      #find all positions related to this iterator
      tagStarts <- grep(regexp, toWrite, perl=T)
      
      if (length(tagStarts) > 0) {
        for (i in 1:length(tagStarts)) {
          #find first foreach closing tag
          endregexp <- paste("\\[\\[\\s*\\/foreach\\s*\\]\\]", sep="")
          #subsets syntax file for only rows after the tag start
          
          tagEnd <- grep(endregexp, toWrite[-1:-tagStarts[i]], perl=T)
          if (length(tagEnd) >= 1) {
            #only keep the first match
            #express tag end in terms of its position after tagStarts (since data subset above)
            tagEnd <- tagEnd[1] + tagStarts[i]
          }
          else if (length(tagEnd) == 0) {
            stop("Could not find matching close tag for foreach tag")
          }
          
          #delete foreach section
          toWrite <- c(toWrite[1:(tagStarts[i]-1)], toWrite[(tagEnd+1):length(toWrite)])
        }
      }
      
      #finally, run replaceTags once more to clean up dynamic tags inserted in the body replace
      toWrite <- replaceTags(regexp=NULL, toWrite, toProcess, perl=T, replaceType="final")
      
      #new method with "final" replace
      filename <- replaceTags(regexp=NULL, toProcess$filename, toProcess, perl=T, replaceType="final")
      print (paste("writing file: ", filename))
      curdir <- getwd()
      
      #figure out the output directory
      outputDir <- replaceTags(regexp=NULL, toProcess$outputDirectory, toProcess, perl=T, replaceType="final")
      
      if (!file.exists(outputDir)) {
        dir.create(outputDir, recursive=T)
      }
      
      setwd(outputDir)
      
      #make sure that no line is more than 90 chars
      toWrite <- unlist(lapply(toWrite, function(line) {
                if (nchar(line) > 90) {
                  strwrap(line, width=85, exdent=5)
                }
                else line
              }))
      
      writeLines(toWrite, con = filename, sep = "\n")
      
      setwd(curdir)
      
    }
  }
}


processInit <- function(initsection) {
  #combine multi-line statements by searching for semi-colon
  assignments <- grep("^\\w+\\s*=", initsection, perl=T)
  
  #preallocate vector of strings to process
  argstoprocess <- vector("character", length(assignments))
  
  #loop through each line containing an assignment
  for (i in 1:length(assignments)) {
    argstoprocess[i] = initsection[assignments[i]]
    
    #if line does not terminate in semicolon, then read subsequent lines until semicolon found
    #start file position at n+1 line
    filepos = assignments[i]+1
    while (length(grep(";\\s*$", argstoprocess[i], perl=T)) != 1) {
      #cat("multi-line: ", unlist(argstoprocess[i]), fill=T)
      argstoprocess[i] = paste(argstoprocess[i], initsection[filepos])
      filepos = filepos+1
    }
  }
  
  arglist <- strsplit(argstoprocess, "\\s*=\\s*", perl=T)
  
  #copy the first element of each vector into the list names
  names(arglist) <- make.names(sapply(arglist, '[', 1))
  
  #now drop the variable names, leaving only values
  #need to preserve as list or it will convert to a vector of strings (b/c drops to one dimension)
  arglist <- as.list(sapply(arglist, '[', -1))
  
  #process any variables that are arrays
  argvals <- as.character(sapply(arglist, '[', 1))
  
  #dump the trailing semicolons (and any trailing spaces)
  argvals <- sub(";\\s*$", "", argvals, perl=T)
  
  for (i in 1:length(argvals)) {
    if (length(grep("^c\\(", argvals[i])) == 1 && length(grep("\\)$", argvals[i])) == 1) {
      #evaluate as an R statement, so create the vector
      arglist[[i]] <- eval(parse(text=argvals[i]))
    }
    else arglist[[i]] <- argvals[i]
  }
  
  if (is.null(arglist$iterate_wrt)) {
    warning("No iterators in init section.")
  }
  
  #convert iterators from string to list
  arglist$iterators <- unlist(strsplit(as.character(arglist$iterate_wrt), "\\s*,\\s*", perl=T))
  
  #process sequence text for each iterator
  for (i in 1:length(arglist$iterators)) {
    seqText <- eval(parse(text=paste("mysplit$", arglist$iterators[i], sep="")))
    if (is.null(seqText)) {
      stop("Iterator specified: ", arglist$iterators[i], ", but not found")
    }
    
    #convert from seq text to actual seq
    arglist[[arglist$iterators[i]]] <- eval(parse(text=seqText))
  }
  
  #default output directory to the current directory
  if (is.null(arglist$outputDirectory)) {
    arglist$outputDirectory <- getwd()
  }
  
  arglist
}
