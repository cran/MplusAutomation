\name{modelParams}
\Rdversion{1.1}
\alias{modelParams}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
modelParams(outfile, filename, extract = c("Title", "LL", "BIC", "AIC", "AICC", "Params", "Observations", "BLRT"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{outfile}{
%%     ~~Describe \code{outfile} here~~
}
  \item{filename}{
%%     ~~Describe \code{filename} here~~
}
  \item{extract}{
%%     ~~Describe \code{extract} here~~
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
function (outfile, filename, extract = c("Title", "LL", "BIC", 
    "AIC", "AICC", "Params", "Observations", "BLRT")) 
{
    startInput <- grep("^\\s*INPUT INSTRUCTIONS\\s*$", outfile, 
        ignore.case = TRUE, perl = TRUE)
    if (length(startInput) == 0) 
        warning("Could not find beginning of input")
    endInput <- grep("^\\s*(INPUT READING TERMINATED NORMALLY|\\d+ WARNING\\(S\\) FOUND IN THE INPUT INSTRUCTIONS)\\s*$", 
        outfile, ignore.case = TRUE, perl = TRUE)
    if (length(endInput) == 0) 
        warning("Could not find end of input")
    processBLRT <- "BLRT" \%in\% extract
    if (processBLRT) {
        extract <- extract[!extract == "BLRT"]
    }
    expandField <- function(name) {
        field <- switch(EXPR = name, Title = c("title: ", "str"), 
            LL = c("H0 Value", "dec"), BIC = c("Bayesian \\(BIC\\)", 
                "dec"), AIC = c("Akaike \\(AIC\\)", "dec"), AICC = c("AICC", 
                "calc"), Params = c("Number of Free Parameters", 
                "int"), aBIC = c("Sample-Size Adjusted BIC", 
                "dec"), Entropy = c("Entropy", "dec"), Observations = c("Number of observations", 
                "int"), CFI = c("CFI", "dec"), TLI = c("TLI", 
                "dec"), c(name, "calc"))
        return(field)
    }
    extractDetailed <- sapply(extract, expandField, USE.NAMES = F)
    arglist = vector("list", length(extract))
    for (i in 1:length(extract)) {
        thisName <- extractDetailed[1, i]
        thisType <- extractDetailed[2, i]
        arglist[i] <- extractValue(name = thisName, outfile, 
            type = thisType)
    }
    names(arglist) <- extract
    if ("LL" \%in\% extract) {
        if (is.na(arglist$LL)) {
            warning(paste("Model missing LL value. Likely a failed run. Dropping from data.\n  ", 
                filename, sep = ""))
            return(NULL)
        }
    }
    if (all(c("AICC", "AIC", "Params", "Observations") \%in\% extract)) {
        arglist$AICC <- arglist$AIC + (2 * arglist$Params * (arglist$Params + 
            1))/(arglist$Observations - arglist$Params - 1)
    }
    if (processBLRT) {
        matchlines <- grep("TECHNICAL 14 OUTPUT", outfile)
        if (length(matchlines) == 1) {
            endRange <- grep("3463 Stoner Ave\\.", outfile)
            if (!length(endRange) == 1) {
                stop("Problem identifying end marker for BLRT")
            }
            blrtpiece <- outfile[matchlines:endRange]
            arglist$KM1LL <- extractValue(name = "H0 Loglikelihood Value", 
                blrtpiece, type = "dec")
            arglist$BLRTp <- extractValue(name = "Approximate P-Value", 
                blrtpiece, type = "dec")
            arglist$BLRTNumdraws <- extractValue(name = "Successful Bootstrap Draws", 
                blrtpiece, type = "int")
        }
        else {
            arglist$KM1LL <- as.numeric(NA)
            arglist$BLRTp <- as.numeric(NA)
            arglist$BLRTNumdraws <- as.numeric(NA)
        }
    }
    arglist$inputInstructions <- paste((outfile[(startInput + 
        1):(endInput - 1)]), collapse = "\n")
    return(as.data.frame(arglist, stringsAsFactors = FALSE))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{interface}