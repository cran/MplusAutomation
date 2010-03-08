\name{extractValue}
\Rdversion{1.1}
\alias{extractValue}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
extractValue(name, outfile, type = "int")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{name}{
%%     ~~Describe \code{name} here~~
}
  \item{outfile}{
%%     ~~Describe \code{outfile} here~~
}
  \item{type}{
%%     ~~Describe \code{type} here~~
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
function (name, outfile, type = "int") 
{
    matchlines <- grep(name, outfile, ignore.case = TRUE, value = TRUE)
    if (type == "calc") {
        return(as.numeric(NA))
    }
    if (length(matchlines) > 1) {
        warning(paste("More than one match found for parameter: ", 
            name, sep = ""))
        return(matchlines)
    }
    else if (length(matchlines) == 0) {
        warning(paste("Parameter not found: ", name, sep = ""))
        return(as.numeric(NA))
    }
    if (type == "int") {
        regexp <- "-*\\d+"
    }
    else if (type == "dec") {
        regexp <- "-*\\d+\\.\\d+"
    }
    else if (type == "str") {
        regexp <- paste(name, ".*", sep = "")
    }
    start <- regexpr(regexp, matchlines[1], perl = TRUE)
    if (type == "str") {
        returnVal <- as.character(sub(name, "", matchlines[1], 
            ignore.case = TRUE))
    }
    else returnVal <- as.numeric(substr(matchlines[1], start, 
        start + attr(start, "match.length")))
    return(returnVal)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{interface}