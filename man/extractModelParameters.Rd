\name{extractModelParameters}
\Rdversion{1.1}
\alias{extractModelParameters}
\title{Extract Model Parameters from MODEL RESULTS section.}
\description{
Extracts the model parameters from the MODEL RESULTS section of a given Mplus output file.
Parameters include the parameter estimate, std. err, param/s.e., and two-tailed p-value.
Model results are returned as a \code{data.frame} object. 
}
\usage{
extractModelParameters(outfile, resultType="raw")
}

\arguments{
  \item{outfile}{the output file to be parsed for model parameters. May be a full path, relative path, or a filename within
  	the working directory. Example: "C:/Users/Michael/Mplus Runs/Model 23.out"}
  \item{resultType}{The results section to extract. If \code{raw}, the unstandardized estimates will be returned.
  	"stdyx", "stdy", and "std" are the other options, which extract different standardized solutions. See the Mplus User's Guide
  	for additional details about the differences in these standardizations.}
}

\value{returns a \code{data.frame} containing the MODEL RESULTS section of \code{outfile}. Variables include
\item{paramHeader}{The header that begins a given parameter set. Example: "FACTOR1 BY"}
\item{param}{The particular parameter being measured (within \code{paramHeader}). Example: "ITEM1"}
\item{paramest}{Parameter estimate value.}
\item{se}{Standard error of the estimate}
\item{est_se}{Quotient of \code{paramest/se}, representing z-test/t-test in large samples}
\item{pval}{Two-tailed p-value for the \code{est_se} quotient.}

If the model contains multiple latent classes, an additional variable, \code{LatentClass},
will be included, specifying the latent class number. Also, the Categorical Latent Variables section
will be included as \code{LatentClass} "Categorical.Latent.Variables."

If the model contains multiple groups, \code{Group} will be included.

If the model contains two-level output (between/within), \code{BetweenWithin} will be included.
}
\author{Michael Hallquist}
\seealso{
\code{\link{extractModelSummaries}}
}
\keyword{interface}