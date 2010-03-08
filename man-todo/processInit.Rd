\name{processInit}
\Rdversion{1.1}
\alias{processInit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
processInit(initsection)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{initsection}{
%%     ~~Describe \code{initsection} here~~
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
function (initsection) 
{
    assignments <- grep("^\\w+\\s*=", initsection, perl = T)
    argstoprocess <- vector("character", length(assignments))
    for (i in 1:length(assignments)) {
        argstoprocess[i] = initsection[assignments[i]]
        filepos = assignments[i] + 1
        while (length(grep(";\\s*$", argstoprocess[i], perl = T)) != 
            1) {
            argstoprocess[i] = paste(argstoprocess[i], initsection[filepos])
            filepos = filepos + 1
        }
    }
    arglist <- strsplit(argstoprocess, "\\s*=\\s*", perl = T)
    names(arglist) <- make.names(sapply(arglist, "[", 1))
    arglist <- as.list(sapply(arglist, "[", -1))
    argvals <- as.character(sapply(arglist, "[", 1))
    argvals <- sub(";\\s*$", "", argvals, perl = T)
    for (i in 1:length(argvals)) {
        if (length(grep("^c\\(", argvals[i])) == 1 && length(grep("\\)$", 
            argvals[i])) == 1) {
            arglist[[i]] <- eval(parse(text = argvals[i]))
        }
        else arglist[[i]] <- argvals[i]
    }
    if (is.null(arglist$iterate_wrt)) {
        warning("No iterators in init section.")
    }
    arglist$iterators <- unlist(strsplit(as.character(arglist$iterate_wrt), 
        "\\s*,\\s*", perl = T))
    for (i in 1:length(arglist$iterators)) {
        seqText <- eval(parse(text = paste("mysplit$", arglist$iterators[i], 
            sep = "")))
        if (is.null(seqText)) {
            stop("Iterator specified: ", arglist$iterators[i], 
                ", but not found")
        }
        arglist[[arglist$iterators[i]]] <- eval(parse(text = seqText))
    }
    if (is.null(arglist$outputDirectory)) {
        arglist$outputDirectory <- getwd()
    }
    arglist
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{interface}