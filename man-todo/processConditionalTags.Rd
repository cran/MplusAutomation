\name{processConditionalTags}
\Rdversion{1.1}
\alias{processConditionalTags}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
processConditionalTags(syntaxFile, toProcess, perl = T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{syntaxFile}{
%%     ~~Describe \code{syntaxFile} here~~
}
  \item{toProcess}{
%%     ~~Describe \code{toProcess} here~~
}
  \item{perl}{
%%     ~~Describe \code{perl} here~~
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
function (syntaxFile, toProcess, perl = T) 
{
    regexp <- paste("\\[\\[\\s*(?:", paste(toProcess$iterators, 
        collapse = "|"), ")\\s*[!><=]+\\s*\\d+\\s*\\]\\]", sep = "")
    tagStarts <- grep(regexp, syntaxFile, perl = T)
    if (length(tagStarts) == 0) {
        return(syntaxFile)
    }
    linesToDrop <- c()
    for (i in 1:length(tagStarts)) {
        regexp <- paste("\\[\\[\\s*((?:", paste(toProcess$iterators, 
            collapse = "|"), ")\\s*[!><=]+\\s*\\d+)\\s*\\]\\]", 
            sep = "")
        tagPart <- unlist(strapply(syntaxFile[tagStarts][i], 
            regexp, c, perl = T, backref = -1))
        for (j in 1:length(tagPart)) {
            endregexp <- paste("\\[\\[\\s*\\/", tagPart[j], "\\s*\\]\\]", 
                sep = "")
            sameLineEnd <- regexpr(endregexp, syntaxFile[tagStarts][i], 
                perl = T)
            if (sameLineEnd > 0) {
                startPos <- regexpr(regexp, syntaxFile[tagStarts][i], 
                  perl = TRUE)
                if (evaluateConditional(tagPart[j], toProcess)) {
                  syntaxFile[tagStarts][i] <- paste(substring(syntaxFile[tagStarts][i], 
                    c(1, (startPos + attr(startPos, "match.length")), 
                      (sameLineEnd + attr(sameLineEnd, "match.length"))), 
                    c((startPos - 1), (sameLineEnd - 1), nchar(syntaxFile[tagStarts][i]))), 
                    collapse = "")
                }
                else {
                  syntaxFile[tagStarts][i] <- paste(substring(syntaxFile[tagStarts][i], 
                    c(1, (sameLineEnd + attr(sameLineEnd, "match.length"))), 
                    c((startPos - 1), nchar(syntaxFile[tagStarts][i]))), 
                    collapse = "")
                }
            }
            else {
                tagEnd <- grep(endregexp, syntaxFile[-1:-tagStarts[i]], 
                  perl = T)
                if (length(tagEnd) >= 1) {
                  tagEnd <- tagEnd[1] + tagStarts[i]
                }
                else if (length(tagEnd) == 0) {
                  stop("Could not find matching close tag for conditional tag: ", 
                    tagPart)
                }
                if (evaluateConditional(tagPart[j], toProcess)) {
                  linesToDrop <- c(linesToDrop, tagStarts[i], 
                    tagEnd)
                }
                else {
                  linesToDrop <- c(linesToDrop, tagStarts[i]:tagEnd)
                }
            }
        }
    }
    syntaxFile <- syntaxFile[!1:length(syntaxFile) \%in\% linesToDrop]
    return(syntaxFile)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{interface}