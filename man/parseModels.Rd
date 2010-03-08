\name{parseModels}
\Rdversion{1.1}
\alias{parseModels}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Parse a group of Mplus models within a directory}
\description{
Parses a group of Mplus model output files (.out extension) for model fit statistics.
At this time, the details extracted are fixed: \code{c("Title", "LL", "BIC", "AIC", "AICC", "Params", "Observations", "BLRT")},
but the infrastructure is in place to allow for user-specified information in future versions.
}
\usage{
parseModels(directory = getwd(), recursive = FALSE, filefilter)
}

\arguments{
  \item{directory}{the directory containing Mplus output files (.out) to parse.
  	Defaults to the current working directory. Example: "C:/Users/Michael/Mplus Runs"}
  \item{recursive}{optional. If \code{TRUE}, parse all models nested in subdirectories
  	within \code{directory}. Defaults to \code{FALSE}.}
  \item{filefilter}{a Perl regular expression (PCRE-compatible) specifying particular output
  	files to be parsed within \code{directory}. See \code{regex} or \url{http://www.pcre.org/pcre.txt}
  	for details about regular expression syntax.}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{Returns a \code{data.frame} containing model fit statistics for all output files within \code{directory}.}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{Michael Hallquist}

\seealso{
\code{\link{regex}}

\code{\link{runModels}}
}
%\examples{
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{interface}