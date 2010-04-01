\name{evaluateConditional}
\Rdversion{1.1}
\alias{evaluateConditional}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
evaluateConditional(tag, toProcess)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tag}{
%%     ~~Describe \code{tag} here~~
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
function (tag, toProcess) 
{
    regexp <- "(\\w+)\\s*([!><=]+)\\s*(\\d+)"
    conditional <- unlist(strapply(tag, regexp, c))
    if (length(conditional) < 3) {
        stop("Error in conditional tag: does not contain variable, operator, and value. Tag = ", 
            tag)
    }
    if (conditional[2] == "=") 
        conditional[2] = "=="
    iteratorPosition <- grep(paste("\\b", conditional[1], "\\b", 
        sep = ""), toProcess$iterators, perl = T)
    return(eval(parse(text = paste("toProcess$curItPos[iteratorPosition]", 
        conditional[2], conditional[3], sep = ""))))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{interface}