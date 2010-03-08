\name{recurseReplace}
\Rdversion{1.1}
\alias{recurseReplace}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
recurseReplace(bodysection, toProcess, curiterator = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bodysection}{
%%     ~~Describe \code{bodysection} here~~
}
  \item{toProcess}{
%%     ~~Describe \code{toProcess} here~~
}
  \item{curiterator}{
%%     ~~Describe \code{curiterator} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (bodysection, toProcess, curiterator = 1) 
{
    if (!is.list(toProcess)) {
        stop("Argument list passed to recurseReplace is not a list")
    }
    thisIterator <- toProcess$iterators[curiterator]
    toProcess$curIteratorName <- thisIterator
    toProcess$curIteratorDepth <- curiterator
    for (i in toProcess[[thisIterator]]) {
        toProcess$curItPos[curiterator] <- i
        print(paste("current iterator is: ", thisIterator, " position:", 
            as.character(i)))
        bodysection <- processForeachTags(bodysection, toProcess)
        if (curiterator < length(toProcess$iterators)) {
            recurseReplace(bodysection, toProcess, curiterator = curiterator + 
                1)
        }
        else {
            toWrite <- processConditionalTags(bodysection, toProcess, 
                perl = T)
            toWrite <- replaceTags(regexp = NULL, toWrite, toProcess, 
                perl = T, replaceType = "final")
            regexp <- paste("\\[\\[\\s*foreach\\s+(", paste(toProcess$iterators, 
                collapse = "|"), ")\\s*\\]\\]", sep = "")
            tagStarts <- grep(regexp, toWrite, perl = T)
            if (length(tagStarts) > 0) {
                for (i in 1:length(tagStarts)) {
                  endregexp <- paste("\\[\\[\\s*\\/foreach\\s*\\]\\]", 
                    sep = "")
                  tagEnd <- grep(endregexp, toWrite[-1:-tagStarts[i]], 
                    perl = T)
                  if (length(tagEnd) >= 1) {
                    tagEnd <- tagEnd[1] + tagStarts[i]
                  }
                  else if (length(tagEnd) == 0) {
                    stop("Could not find matching close tag for foreach tag")
                  }
                  toWrite <- c(toWrite[1:(tagStarts[i] - 1)], 
                    toWrite[(tagEnd + 1):length(toWrite)])
                }
            }
            toWrite <- replaceTags(regexp = NULL, toWrite, toProcess, 
                perl = T, replaceType = "final")
            filename <- replaceTags(regexp = NULL, toProcess$filename, 
                toProcess, perl = T, replaceType = "final")
            print(paste("writing file: ", filename))
            curdir <- getwd()
            outputDir <- replaceTags(regexp = NULL, toProcess$outputDirectory, 
                toProcess, perl = T, replaceType = "final")
            if (!file.exists(outputDir)) {
                dir.create(outputDir, recursive = T)
            }
            setwd(outputDir)
            toWrite <- unlist(lapply(toWrite, function(line) {
                if (nchar(line) > 90) {
                  strwrap(line, width = 85, exdent = 5)
                }
                else line
            }))
            writeLines(toWrite, con = filename, sep = "\n")
            setwd(curdir)
        }
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{interface}