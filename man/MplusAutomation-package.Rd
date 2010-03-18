\name{MplusAutomation-package}
\Rdversion{1.1}
\alias{MplusAutomation-package}
\alias{MplusAutomation}
\docType{package}
\title{Automating Mplus Model Estimation and Interpretation}
\description{
The MplusAutomation package leverages the flexibility of the 
	R language to automate latent variable model estimation and interpretation using Mplus, 
	a powerful latent variable modeling program developed by Muthen and Muthen (www.statmodel.com).
	Specifically, MplusAutomation provides routines for creating related groups of models,
	running batches of models, and extracting and tabulating model parameters and fit statistics.}
\details{
\tabular{ll}{
Package: \tab MplusAutomation\cr
Type: \tab Package\cr
Version: \tab 0.2-4\cr
Date: \tab 2010-03-18\cr
License: \tab LGPL-3\cr
LazyLoad: \tab yes\cr
}

The MplusAutomation package has three primary purposes:
\enumerate{
	\item To automatically run groups/batches of models.
	\item To provide routines to extract model fit statistics, parameter estimates,
		and raw data from Mplus output files.
	\item To provide a template language that allows for the creation of related input files (under development).
}

At this time, the first two items are included in the package, with a prototype of the template language under development.

The core routine for running batches of models is \code{\link{runModels}}, with an easy-to-use GUI wrapper, \code{\link{runModels_Interactive}}.

To extract model summary statistics from one or more output files, see \code{\link{extractModelSummaries}}, which returns a \code{data.frame} of fit statistics
for models located within a directory. Model fit results can be summarized in tabular form (for comparing among models) using \code{\link{showSummaryTable}}
(displays table in separate window), \code{\link{HTMLSummaryTable}} (creates HTML file containing summary table), or \code{\link{LatexSummaryTable}} (returns
a LaTex-formatted table of summary statistics).

To extract raw data created by the SAVEDATA command (e.g., class membership probabilities or factor scores), see \code{\link{getSavedata_Data}}.

To extract unstandardized or standardized parameter estimates from a single output file, see \code{\link{extractModelParameters}}.
}
\author{
Michael Hallquist

Maintainer: Michael Hallquist <michael.hallquist@gmail.com>
}
\references{
Mplus software. Muthen and Muthen. \url{http://www.statmodel.com} 
}
\keyword{ package }
%\seealso{
%~~ Optional links to other man pages, e.g. ~~
%~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
%}
%\examples{}
