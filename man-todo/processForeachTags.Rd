\name{processForeachTags}
\Rdversion{1.1}
\alias{processForeachTags}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
processForeachTags(syntaxFile, toProcess)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{syntaxFile}{
%%     ~~Describe \code{syntaxFile} here~~
}
  \item{toProcess}{
%%     ~~Describe \code{toProcess} here~~
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
function (syntaxFile, toProcess) 
{
    regexp <- paste("\\[\\[\\s*foreach\\s+", toProcess$curIteratorName, 
        "\\s*\\]\\]", sep = "")
    tagStarts <- grep(regexp, syntaxFile, perl = T)
    if (length(tagStarts) == 0) {
        return(syntaxFile)
    }
    for (i in 1:length(tagStarts)) {
        endregexp <- paste("\\[\\[\\s*\\/foreach\\s*\\]\\]", 
            sep = "")
        tagEnd <- grep(endregexp, syntaxFile[-1:-tagStarts[i]], 
            perl = T)
        if (length(tagEnd) >= 1) {
            tagEnd <- tagEnd[1] + tagStarts[i]
        }
        else if (length(tagEnd) == 0) {
            stop("Could not find matching close tag for foreach tag for iterator: ", 
                toProcess$curIteratorName)
        }
        workingSection <- syntaxFile[(tagStarts[i] + 1):(tagEnd - 
            1)]
        workingSection <- processConditionalTags(workingSection, 
            toProcess, perl = T)
        workingSection <- replaceTags(regexp = NULL, workingSection, 
            toProcess, perl = T, replaceType = "array")
        workingSection <- replaceTags(regexp = NULL, workingSection, 
            toProcess, perl = T, replaceType = "iterator")
        if (toProcess$curItPos[toProcess$curIteratorDepth] < 
            max(toProcess[[toProcess$curIteratorName]])) {
            syntaxFile <- c(syntaxFile[1:(tagStarts[i] - 1)], 
                workingSection, syntaxFile[tagStarts[i]:length(syntaxFile)])
        }
        else {
            syntaxFile <- c(syntaxFile[1:(tagStarts[i] - 1)], 
                workingSection, syntaxFile[(tagEnd + 1):length(syntaxFile)])
        }
        return(syntaxFile)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{interface}