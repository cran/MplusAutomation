\name{createModels}
\Rdversion{1.1}
\alias{createModels}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Create Mplus Input Files from Template}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
createModels(templatefile, run = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{templatefile}{
%%     ~~Describe \code{templatefile} here~~
}
  \item{run}{
%%     ~~Describe \code{run} here~~
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
function (templatefile, run = F) 
{
    if (!file.exists(templatefile)) 
        stop("Template file not found.")
    readfile <- scan(templatefile, what = "character", sep = "\n", 
        strip.white = FALSE)
    startinit <- grep("[[init]]", readfile, fixed = T)
    endinit <- grep("[[/init]]", readfile, fixed = T)
    if (length(startinit) != 1 || length(endinit) != 1) {
        stop("Unable to find init section in template file.")
    }
    initsection <- readfile[(startinit + 1):(endinit - 1)]
    bodysection <- readfile[(endinit + 1):length(readfile)]
    toProcess <- processInit(initsection)
    toProcess$.allTags <- grep("\\[\\[\\s*[\\w=><!#]+\\s*\\]\\]", 
        bodysection, perl = T)
    regexp <- paste("\\[\\[\\s*(?!", paste(toProcess$iterators, 
        collapse = "|"), ")\\w+\\s*\\]\\]", sep = "")
    toProcess$.simpleTags <- grep(regexp, bodysection[toProcess$.allTags], 
        perl = T)
    bodysection[toProcess$.allTags][toProcess$.simpleTags] <- replaceTags(regexp, 
        bodysection[toProcess$.allTags][toProcess$.simpleTags], 
        toProcess, replaceType = "simple")
    if (length(toProcess$iterators) > 0) {
        recurseReplace(bodysection, toProcess)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{interface}