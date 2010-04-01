#SOME THOUGHTS RE DOCUMENTATION
#foreach tags may only be with respect to an iterator... could not have some random foreach var
friendlyGregexpr <- function(pattern, charvector, perl=TRUE) {
  require(plyr)
  #now create data frame documenting the start and end of all tags
  #rather than ldply, need a usual loop to track element number (in cases where charvector is a vector)
  regexpMatches <- gregexpr(pattern, charvector, perl=perl)
  
  convertMatches <- c()
  for (i in 1:length(regexpMatches)) {
    thisLine <- regexpMatches[[i]]
    #only append if there is at least one match on this line
    if (thisLine[1] != -1) {
      convertMatches <- rbind(convertMatches, data.frame(element=i, start=thisLine, end=thisLine+attr(thisLine, "match.length")-1))
    }
  }
  
  #if no matches exist, return null (otherwise, will break adply
  if (is.null(convertMatches)) return(NULL)
  
  #okay, now we have a data frame with the line, starting position, and ending position of every tag
  
  #time to classify into simple, array, iterator, and conditional
  
  #first, add the actual tag to the data.frame to make it easier to parse
  #using adply (is this not its intended use?) to iterate over rows and apply func
  convertMatches <- adply(convertMatches, 1, function(row) {
        row$tag <- substr(charvector[row$element], row$start, row$end)
        return(as.data.frame(row))        
      })
  
  return(convertMatches)
}


classifyTags <- function(tagVector, iteratorsVector) {
  #accepts a vector of tags to be classified
  #also needs a vector of the iterators to correctly classify tags
  #returns a vector of tag types
  
  #creates an empty character vector of the same length as tagVector (each element defaults to "") 
  tagType <- vector(mode="character", length=length(tagVector))

  #default to missing for tag type (replaced below)
  #tagData$tagType <- NA_character_
  
  iteratorsRegEx <- paste("\\[\\[\\s*(", paste(iteratorsVector, collapse="|"), ")\\s*\\]\\]", sep="")
  iteratorPositions <- grep(iteratorsRegEx, tagVector, perl=T)
  tagType[iteratorPositions] <- "iterator"
  
  arrayRegEx <- paste("\\[\\[\\s*\\b([\\w\\.]+)#(", paste(iteratorsVector, collapse="|"), ")\\b\\s*\\]\\]", sep="")
  arrayPositions <- grep(arrayRegEx, tagVector, perl=T)
  tagType[arrayPositions] <- "array"
  
  #optional forward slash for closing tags
  #could the alternation syntax be problematic if variable names overlaps (e.g., x matching xy)? Use word boundaries?
  #any reason to limit this to iterators?!
  
  conditionalRegEx <- paste("\\[\\[\\s*/*(", paste(iteratorsVector, collapse="|"), ")\\s*[!><=]+\\s*\\d+\\s*\\]\\]", sep="")
  conditionalPositions <- grep(conditionalRegEx, tagVector, perl=T)
  tagType[conditionalPositions] <- "conditional"
  
  #simple tags -- not wrt iterators, not conditional
  #use negative lookahead to skip tags that are iterators
  simpleRegEx <- paste("\\[\\[\\s*(?!", paste(iteratorsVector, collapse="|"), ")[\\w+\\.]+\\s*\\]\\]", sep="")  
  simplePositions <- grep(simpleRegEx, tagVector, perl=T)
  tagType[simplePositions] <- "simple"
  
  return(tagType)
}

getInitTags <- function(initCollection) {
  initMatches <- c()
  for (i in 1:length(initCollection)) {
    if (storage.mode(initCollection[[i]]) == "character") {
      matches <- friendlyGregexpr("\\[\\[\\s*[\\s\\w=><!#/]+\\s*\\]\\]", initCollection[[i]], perl=T)
      #if there are matches for this item, add its position in the list pos
      #the idea is that the list has elements and the elements can be vectors
      #thus, a match may occur for initCollection[[5]][3] if the fifth element of the list is a vector
      #and the match is the third element.
      if (!is.null(matches)) matches$listpos <- i
      initMatches <- rbind(initMatches, matches)
    }
  }
  
  #successfully creates a data.frame of the sort below.
#   element start end                          tag listpos
#1        1     1  11                  [[classes]]      14
#2        1    19  38         [[groupnames#group]]      14
#3        1    40  63     [[outcomenames#outcome]]      14
#4        1    65  84         [[modelnames#model]]      14
#5        1    85 112 [[zeroclassnames#zeroclass]]      14
#6        1     6  29     [[outcomenames#outcome]]      15
#7        1    31  50         [[groupnames#group]]      15
#8        1    73  92         [[modelnames#model]]      15
#9        1     1   9                    [[hello]]      17
#10       2     1  10                   [[hello2]]      17
  
  #classify tags in terms of simple, array, iterator, conditional, foreach 
  if (nrow(initMatches) > 0) {
    initMatches$tagType <- classifyTags(initMatches$tag, initCollection$iterators)
    
    #chop off the [[ ]] portion of the tags, along with any leading or trailing space
    #this makes it easier to use the sub function to update current values   
    initMatches$tag <- sapply(initMatches$tag, function(tag) {
          return(sub("\\[\\[\\s*([\\s\\w=><!#/]+)\\s*\\]\\]", "\\1", tag, perl=TRUE))
        })
  }
  
  #return empty data frame if no matches 
  if (is.null(initMatches)) return(data.frame())
  else return(initMatches)
}

parseTags <- function(bodySection, initCollection) {
  #parses tags in the body section (character vector) and
  #init collection (list of vars defined in the init section
  #returns a list with $initTags and $bodyTags
  #where each list represents the location, start character, end character, tag type, etc.
  #of each tag
  
  #first handle init tags

  #initCollection$testy <- c("[[hello]]", "[[hello2]]")
  initMatches <- getInitTags(initCollection)
  
  initMatches$currentValue <- NA_character_

  
  #BODY TAG SECTION
  #for temporary testing
  #bodysection <- scan("C:/Users/Michael Hallquist/Documents/SampleBody.txt", what="character", sep="\n", strip.white=FALSE, blank.lines.skip=FALSE)
  #initCollection <- data.frame(iterators=c("group", "classes", "outcome", "model", "zeroclass"))
  
  bodyTagRegex <- "\\[\\[\\s*[\\s\\w=><!#/]+\\s*\\]\\]"
  bodyMatches <- friendlyGregexpr(bodyTagRegex, bodySection, perl=TRUE)
  
  bodyMatches$tagType <- classifyTags(bodyMatches$tag, initCollection$iterators)  
  #okay, now every tag is categorized
  #the notion here is to substitute in the running value for a given variable
  #then we'll do a mass substitute for each model
  bodyMatches$currentValue <- NA_character_
  
  #chop off the [[ ]] portion of the tags, along with any leading or trailing space
  bodyMatches$tag <- sapply(bodyMatches$tag, function(tag) {
        return(sub("\\[\\[\\s*([\\s\\w=><!#/]+)\\s*\\]\\]", "\\1", tag, perl=TRUE))
      })
  
 
  #return a two-element list with constituent data frames for init and body tags.
  return(list(initTags=initMatches, bodyTags=bodyMatches))  
  
}

#createModels("C:/Users/Michael Hallquist/Documents/Online_Documents/Postdoc/Mplus Automation/LSPD Covariate Template.txt")
createModels <- function(templatefile, run=F) {
  if (!file.exists(templatefile)) stop("Template file not found.")
  
  readfile <- scan(templatefile, what="character", sep="\n", strip.white=FALSE)
  
  #divide into init versus body
  startinit <- grep("[[init]]", readfile, fixed=T)
  endinit <- grep("[[/init]]", readfile, fixed=T)
  
  if (length(startinit) != 1 || length(endinit) != 1) {
    stop("Unable to find init section in template file.")
  }
  
  #extract init section
  initSection <<- readfile[(startinit+1):(endinit-1)]
  
  #extract body section
  bodySection <<- readfile[(endinit+1):length(readfile)]
  
  #convert the init text into a list object containing parsed init instructions
  initCollection <<- processInit(initSection)
  
  templateTags <<- parseTags(bodySection, initCollection)
  
  #lookup values for simple tags
  templateTags <<- lookupSimpleTags(templateTags, initCollection) 
  
 
  #kick off the recursive replace
  if (length(initCollection$iterators) > 0) {
    recurseReplace(bodySection, templateTags, initCollection)
  }
}

lookupSimpleTags <- function(templateTags, initCollection) {
  #the purpose of this function is to set the currentValue column
  #for the bodyTags and initTags data.frames for simple tags only.
  #Most values will be replaced at the bottom level of recursion,
  #but simple tags do not change over iterations, so can be set one time.
  
  #locate simple tags in body
  simpleBodyPositions <- which(templateTags$bodyTags$tagType=="simple")

  #replace tag with value
  templateTags$bodyTags$currentValue[simpleBodyPositions] <- sapply(templateTags$bodyTags$tag[simpleBodyPositions],
      function(value) {
        currentValue <- eval(parse(text=paste("initCollection$", value, sep="")))
        if (regexpr("\\[\\[\\s*[\\s\\w=><!#/]+\\s*\\]\\]", currentValue, perl=TRUE) > 0) {
          #The replacement tag itself contains additional tags.
          #Thus, not a simple replacement. This replacement needs to be deferred until
          #we have iterated to the bottom of the tree and have all needed information
          #set a deferred value to be replace later
          currentValue <- "..deferred.."
        }
        return(currentValue)
      })

  #locate simple tags in init
  simpleInitPositions <- which(templateTags$initTags$tagType=="simple")
  
  templateTags$initTags$currentValue[simpleInitPositions] <- sapply(templateTags$initTags$tag[simpleInitPositions],
      function(value) {
        return(eval(parse(text=paste("initCollection$", value, sep=""))))
      })
  
  return(templateTags)
  
}

updateCurrentValues <- function(templateTags, initCollection) {
  #need to replace array and iterator tags for this iterator
  
#  manual settings for testing
#  initCollection$curIteratorName <- "outcome"
#  initCollection$curItPos[1] <- 1
#  initCollection$curIteratorDepth <- 1
  
  #initCollection$curIteratorName <- "classes"
  #initCollection$curIteratorDepth <- 4
  #initCollection$curItPos[4] <- 4
  
  #locate iterator tags in init
  initIteratorPositions <- which(templateTags$initTags$tagType=="iterator" & templateTags$initTags$tag == initCollection$curIteratorName)
  
  #set the current value to the position in the looping process for this iterator
  templateTags$initTags$currentValue[initIteratorPositions] <- initCollection$curItPos[initCollection$curIteratorDepth]

  #locate iterator tags in body
  bodyIteratorPositions <- which(templateTags$bodyTags$tagType=="iterator" & templateTags$bodyTags$tag == initCollection$curIteratorName)
  
  templateTags$bodyTags$currentValue[bodyIteratorPositions] <- initCollection$curItPos[initCollection$curIteratorDepth]

  #Next, handle array tags
  #figure out the iterator for each array tag and only select those that are relevant to the current iterator
  initArrayPositions <- which(templateTags$initTags$tagType=="array")

  #use plyr's splitter_a function to divide dataset by row (builds a big list)
  divideByRow <- splitter_a(templateTags$initTags[initArrayPositions,], 1)
  
  #for each element of the list, check for a match with this iterator and return the value of interest
  #if the array tag is not for this iterator, return the current value unchanged
  templateTags$initTags$currentValue[initArrayPositions] <- unlist(sapply(divideByRow,
      function(row) {
        split <- strsplit(row$tag, split="#", fixed=TRUE)[[1]]
        if (length(split) != 2) stop("array tag missing iterator: ", row$tag)
        
        if (split[2] == initCollection$curIteratorName) {
          currentValue <- eval(parse(text=paste("initCollection$", split[1], "[", initCollection$curItPos[initCollection$curIteratorDepth], "]", sep="")))
          if (is.null(currentValue)) stop("When replacing tag: ", row$tag, ", could not find corresponding value.")
          return(currentValue)
        }
        else return(row$currentValue) #return unchanged current value if not this iterator
      }))

  #conduct same process for body tags: locate array tags and update values for this iterator
  bodyArrayPositions <- which(templateTags$bodyTags$tagType=="array")
  
  #use plyr's splitter_a function to divide dataset by row (builds a big list)
  divideByRow <- splitter_a(templateTags$bodyTags[bodyArrayPositions,], 1)
  
  #for each element of the list, check for a match with this iterator and return the value of interest
  templateTags$bodyTags$currentValue[bodyArrayPositions] <- unlist(sapply(divideByRow,
      function(row) {
        split <- strsplit(row$tag, split="#", fixed=TRUE)[[1]]
        if (length(split) != 2) stop("array tag missing iterator: ", row$tag)
        
        if (split[2] == initCollection$curIteratorName) {
          currentValue <- eval(parse(text=paste("initCollection$", split[1], "[", initCollection$curItPos[initCollection$curIteratorDepth], "]", sep="")))
          if (regexpr("\\[\\[\\s*[\\s\\w=><!#/]+\\s*\\]\\]", currentValue, perl=TRUE) > 0) {
            #The replacement tag itself contains additional tags.
            #Thus, not a simple replacement. This replacement needs to be deferred until
            #we have iterated to the bottom of the tree and have all needed information
            #set a deferred value to be replace later
            currentValue <- "..deferred.."
          }
          if (is.null(currentValue)) stop("When replacing tag: ", row$tag, ", could not find corresponding value.")
          return(currentValue)
        }
        else return(row$currentValue) #return unchanged current value if not this iterator
      }))

  return(templateTags)
}


#in the process of updating the recurseReplace function

recurseReplace <- function(bodySection, templateTags, initCollection, curiterator=1) {
  #bodySection is the character vector representing each line of the body section
  #bodyTags is a data.frame documenting the location and type of all tags in bodySection
  #initCollection is the list of all arguments parsed from the init section
  #initTags is a data.frame documenting the location and type of all tags in initCollection
  #curiterator is an integer that tracks of the depth of recursion through the iterators
  
  if (!is.list(initCollection)) {
    stop("Argument list passed to recurseReplace is not a list")
  }
  
  thisIterator <- initCollection$iterators[curiterator]
  
  #set the current iterator for the collection (used by replaceTags)
  initCollection$curIteratorName <- thisIterator
  initCollection$curIteratorDepth <- curiterator
  
  for (i in initCollection[[thisIterator]]) {
    
    #set the current position within this iterator for use in replace tags
    #create a vector of iterator positions for use in replaceTags
    initCollection$curItPos[curiterator] <- i
    print(paste("current iterator is: ", thisIterator, " position:", as.character(i)))
    
    #process foreach commands
    #For now, take this out
    #bodySection <- processForEachTags(bodySection, initCollection)
    
    #update the current values for this iterator and this iteration
    #this applies for every iterator and iteration, not just processing
    #at the deepest level. The function only updates array and iterator
    #tags that match this iterator, thus minimizing redundant work.
    templateTags <- updateCurrentValues(templateTags, initCollection)
    
    
    if (curiterator < length(initCollection$iterators)) {
      #if not at deepest level, recurse to the next level by adding 1 to the iterator
      
      #NOTE to self: consider adding a "foreachReplacements" collection to templateTags
      #that contains the expansions of these tags (appended per iteration)
      #this avoids having to think about reparsing the tags based on new code created by foreach
          
      recurseReplace(bodySection, templateTags, initCollection, curiterator = curiterator+1)
    }
    else {
      #we have reached the bottom of the iteration tree
      #simple, array, and iterator tags should be up to date in the templateTags collection 
      
      #first delete conditional tags from the body section, reduce subsequent processing burden
      #toWrite <- processConditionalTags(bodySection, templateTags, initCollection)
  
      #update bodySection with tag values
      toWrite <- replaceTags(bodySection, templateTags, initCollection)
      print(toWrite)
      return(NULL)
      
      #need to process conditional tags
      finalTemplateTags <- processConditionalTags(templateTags, initCollection)
  
      #don't overwrite bodySection, which should retain its tags
      #process conditional tags first because it may alter the content of the tags to be parsed
      toWrite <- replaceTags(bodySection, bodyTags, initCollection, initTags)    
      
      toWrite <- processConditionalTags(bodySection, initCollection, perl=T)
      toWrite <- replaceTags(regexp=NULL, toWrite, initCollection, perl=T, replaceType="final")
      
      #kludgy but necessary: clean up foreach tags
      regexp <- paste("\\[\\[\\s*foreach\\s+(", paste(initCollection$iterators, collapse="|"), ")\\s*\\]\\]", sep="")
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
      toWrite <- replaceTags(regexp=NULL, toWrite, initCollection, perl=T, replaceType="final")
      
      #new method with "final" replace
      filename <- replaceTags(regexp=NULL, initCollection$filename, initCollection, perl=T, replaceType="final")
      print (paste("writing file: ", filename))
      curdir <- getwd()
      
      #figure out the output directory
      outputDir <- replaceTags(regexp=NULL, initCollection$outputDirectory, initCollection, perl=T, replaceType="final")
      
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

replaceTags <- function(bodySection, templateTags, initCollection) {
  if (length(bodySection) <= 0) stop("Empty body section")
  
  #need to ponder issues where a replaced tag still contains another tag
  
  #hmm, actually seems futile to do a replacement in the init section
  #these are already set by update values.... won't affect the body section
  
  initTagRows <- which(templateTags$initTags$tagType %in% c("simple", "iterator", "array"))
  initTags <- templateTags$initTags[initTagRows,]
  initTags$rownumber <- 1:nrow(initTags)
  
  #going to re-use this chunk in finalizeSubstitutions, so functionalize... consider the looping replacement here
  for (i in 1:nrow(initTags)) {
    row <- initTags[i,]
    stringToChange <- initCollection[[row$listpos]][row$element]

    if(row$start > 1) preTag <- substr(stringToChange, 1, row$start-1)
    else preTag <- ""

    if(row$end < nchar(stringToChange)) postTag <- substr(stringToChange, row$end+1, nchar(stringToChange))
    else postTag <- ""

    initCollection[[row$listpos]][row$element] <- paste(preTag, row$currentValue, postTag, sep="")

    #need to offset subsequent start/stops by the difference between the tag and replacement lengths
    diffLength <- nchar(row$currentValue) - (row$end - row$start + 1)
    
    subsequentRows <- which(initTags$rownumber > i & initTags$listpos==row$listpos & initTags$element==row$element)
    
    if (length(subsequentRows > 0)) {
      #update rows in initTags that have additional tags on the same row
      #need to offset by the diffLength
      initTags[subsequentRows,"start"] <- initTags[subsequentRows,"start"] + diffLength
      initTags[subsequentRows,"end"] <- initTags[subsequentRows,"end"] + diffLength
    }
  }
  
  #refresh the templateTags collection with the replaced values
  #need to dump the rownumber to align the data.frames (templateTags doesn't have a rownumber field)
  initTags$rownumber <- NULL
  templateTags$initTags[initTagRows,] <- initTags
  browser()

  #cleanup deferred tags
  #"..deferred.."

# so we need to finalize the tag substitutions...
# the idea is that we need to convert all tags to literals in the initCollection
# once this is done, then we replace all deferred tags in the body section
  
    
  #don't update current values if initcollection value contains any tag
  #if so, replace at the last minute (check this in Init)
  
  #set a "deferred" status in currentValue if replacement contains tags
  
  bodyTags <- subset(templateTags$bodyTags, tagType %in% c("simple", "iterator", "array"))
  bodyTags$rownumber <- 1:nrow(bodyTags)
  
  deferredTags <- subset(bodyTags, currentValue=="..deferred..")
  
  for(i in 1:nrow(deferredTags)) {
    #for deferred tags, we need to lookup the final values based on the init collection values
    
  }
  
  print(bodyTags)
  stop("test")
  
  for (i in 1:nrow(bodyTags)) {
    row <- bodyTags[i,]
    stringToChange <- bodySection[row$element]
    
    if(row$start > 1) preTag <- substr(stringToChange, 1, row$start-1)
    else preTag <- ""
    
    if(row$end < nchar(stringToChange)) postTag <- substr(stringToChange, row$end+1, nchar(stringToChange))
    else postTag <- ""
    
    bodySection[row$element] <- paste(preTag, row$currentValue, postTag, sep="")

    #need to offset subsequent start/stops by the difference between the tag and replacement lengths
    diffLength <- nchar(row$currentValue) - (row$end - row$start + 1)
    
    subsequentRows <- which(bodyTags$rownumber > i & bodyTags$element==row$element)

    if (length(subsequentRows > 0)) {
      #update rows in bodyTags that have additional tags on the same row
      #need to offset by the diffLength
      bodyTags[subsequentRows,"start"] <- bodyTags[subsequentRows,"start"] + diffLength
      bodyTags[subsequentRows,"end"] <- bodyTags[subsequentRows,"end"] + diffLength
    }
  }
  
  return(bodySection)
}

finalizeTagSubstitutions <- function(templateTags, initCollection) {
  #this function should handle initTags that still contain tags
  #once the initCollection is finalized, then process the deferred body tags
  #the notion is that the substitutions will be handled in an inefficient manner -- using lots
  #of regular expression parsing, not using the matched tags data.frame
  
  #we only need to handle simple and array tags
  #iterators should always be integers
  #foreach and conditional are not relevant
  
  #iterate over init tags until no tags are left
  
  tagsRemain <- TRUE
  numIterations <- 1
  while(tagsRemain) {
    initTags <- getInitTags(initCollection)
    
    if (nrow(initTags) == 0) break #if no tags found, then substitution complete
    
    initTags <- subset(initTags, tagType %in% c("simple", "array"))
    if (nrow(initTags) == 0) break #some tags, but none of the simple or array variety, which we want to replace

    #use plyr's splitter_a function to divide dataset by row (builds a big list)
    divideByRow <- splitter_a(initTags, 1)
    
    #for each element of the list, check for a match with this iterator and return the value of interest
    #if the array tag is not for this iterator, return the current value unchanged
    initTags$currentValue <- unlist(sapply(divideByRow,
        function(row) {
          if (row$tagType == "simple") {
            return(eval(parse(text=paste("initCollection$", tag, sep=""))))
          }
          else if (row$tagType == "array") {
            split <- strsplit(row$tag, split="#", fixed=TRUE)[[1]]
            if (length(split) != 2) stop("array tag missing iterator: ", row$tag)
            
            #find where in the iterator depth this iterator lies
            iteratorPosition <- grep(paste("\\b", split[2], "\\b", sep=""), initCollection$iterators, perl=T)
            
            currentValue <- eval(parse(text=paste("initCollection$", split[1], "[", initCollection$curItPos[iteratorPosition], "]", sep="")))
            if (is.null(currentValue)) stop("When replacing tag: ", row$tag, ", could not find corresponding value.")
            return(currentValue)
          }
        }
    ))

    
    
    numIterations <- numIterations + 1
    if (numIterations > 50) stop("While replacing tags in init section, looped over variables 50 times without completing substitutions. Check for circular definitions.") 
  }
  
  #handles simple tags
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
  
  #handles array tags
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
  
}

processConditionalTags <- function(bodySection, templateTags, initCollection) {

  #find all conditional tags in the body section and remove them from the templateTags and bodySection pieces...
  #how will I return both??
  
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
    seqText <- eval(parse(text=paste("arglist$", arglist$iterators[i], sep="")))
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
  
  return(arglist)
}
