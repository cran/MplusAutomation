\name{prepareMplusData}
\Rdversion{1.1}
\alias{prepareMplusData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Create tab-delimited file and Mplus input syntax from R data.frame}
\description{
The \code{prepareMplusData} function converts an R data.frame object into a tab-delimited file (without header)
to be used in an Mplus input file. The corresponding Mplus syntax, including the data file definition and
variable names, is printed to the console.
}
\usage{
prepareMplusData(df, filename, keepCols, dropCols)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{The R data.frame to be prepared for Mplus}
  \item{filename}{The path and filename for the tab-delimited data file for use with Mplus. Example: "C:/Mplusdata/data1.dat"}
  \item{keepCols}{A character vector specifying the variable names within \code{df} to be output to \code{filename}.}
  \item{dropCols}{A character vector specifying the variable names within \code{df} to be omitted from the data output to \code{filename}.}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{No value is returned by this function. It is solely used to convert an R data.frame to an Mplus data file.}
\author{Michael Hallquist}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{interface}