\name{replaceTags}
\Rdversion{1.1}
\alias{replaceTags}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
replaceTags(regexp = NULL, syntaxFile, toProcess, perl = T, replaceType)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{regexp}{
%%     ~~Describe \code{regexp} here~~
}
  \item{syntaxFile}{
%%     ~~Describe \code{syntaxFile} here~~
}
  \item{toProcess}{
%%     ~~Describe \code{toProcess} here~~
}
  \item{perl}{
%%     ~~Describe \code{perl} here~~
}
  \item{replaceType}{
%%     ~~Describe \code{replaceType} here~~
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
function (regexp = NULL, syntaxFile, toProcess, perl = T, replaceType) 
{
    if (!is.list(toProcess)) {
        stop("Argument list passed to replaceTags is not a list")
    }
    if (length(syntaxFile) == 0) {
        return(NA)
    }
    if (replaceType == "simple" || replaceType == "final") {
        regexp <- paste("\\[\\[\\s*(?!", paste(toProcess$iterators, 
            collapse = "|"), ")(\\w+)\\s*\\]\\]", sep = "")
        tags <- gsubfn(regexp, function(tag, simpleWord) {
            replaceText <- eval(parse(text = paste("toProcess$", 
                simpleWord, sep = "")))
            if (is.null(replaceText)) {
                stop("When replacing tag: ", tag, ", could not find corresponding value.")
            }
            replaceText
        }, syntaxFile, perl = perl, backref = 1)
    }
    else if (replaceType == "array") {
        regexp <- paste("\\[\\[\\s*(\\w+)#", toProcess$curIteratorName, 
            "\\s*\\]\\]", sep = "")
        tags <- gsubfn(regexp, function(tag, arrayMatch) {
            replaceText <- eval(parse(text = paste("toProcess$", 
                arrayMatch, "[", toProcess$curItPos[toProcess$curIteratorDepth], 
                "]", sep = "")))
            if (is.null(replaceText)) {
                stop("When replacing tag: ", tag, ", could not find corresponding value.")
            }
            replaceText
        }, syntaxFile, perl = perl, backref = 1)
    }
    else if (replaceType == "iterator") {
        regexp <- paste("\\[\\[\\s*(", toProcess$curIteratorName, 
            ")\\s*\\]\\]", sep = "")
        tags <- gsubfn(regexp, function(tag, arrayMatch) {
            replaceText <- as.character(toProcess$curItPos[toProcess$curIteratorDepth])
            if (is.null(replaceText)) {
                stop("When replacing tag: ", tag, ", could not find corresponding value.")
            }
            replaceText
        }, syntaxFile, perl = perl, backref = 1)
    }
    if (replaceType == "final") {
        regexp <- paste("\\[\\[\\s*(", paste(toProcess$iterators, 
            collapse = "|"), ")\\s*\\]\\]", sep = "")
        tags <- gsubfn(regexp, function(tag, arrayMatch) {
            iteratorPosition <- grep(paste("\\b", arrayMatch, 
                "\\b", sep = ""), toProcess$iterators, perl = T)
            if (length(iteratorPosition) == 1) {
                replaceText <- as.character(toProcess$curItPos[iteratorPosition])
            }
            if (is.null(replaceText)) {
                stop("When replacing tag: ", tag, ", could not find corresponding value.")
            }
            replaceText
        }, tags, perl = perl, backref = 1)
        regexp <- paste("\\[\\[\\s*\\b(\\w+)#(", paste(toProcess$iterators, 
            collapse = "|"), ")\\b\\s*\\]\\]", sep = "")
        tags <- gsubfn(regexp, function(tag, arrayMatch, iteratorMatch) {
            iteratorPosition <- grep(paste("\\b", iteratorMatch, 
                "\\b", sep = ""), toProcess$iterators, perl = T)
            if (length(iteratorPosition) == 1) {
                replaceText <- eval(parse(text = paste("toProcess$", 
                  arrayMatch, "[", toProcess$curItPos[iteratorPosition], 
                  "]", sep = "")))
            }
            if (is.null(replaceText)) {
                stop("When replacing tag: ", tag, ", could not find corresponding value.")
            }
            replaceText
        }, tags, perl = perl, backref = 2)
    }
    tags
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{interface}