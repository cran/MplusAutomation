% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mplus.R
\name{mplusModeler}
\alias{mplusModeler}
\title{Create, run, and read Mplus models.}
\usage{
mplusModeler(
  object,
  dataout,
  modelout,
  run = 0L,
  check = FALSE,
  varwarnings = TRUE,
  Mplus_command = detectMplus(),
  writeData = c("ifmissing", "always", "never"),
  hashfilename = TRUE,
  killOnFail = TRUE,
  quiet = TRUE,
  ...
)
}
\arguments{
\item{object}{An object of class mplusObject}

\item{dataout}{the name of the file to output the data to for Mplus.
If missing, defaults to \code{modelout} changing .inp to .dat.}

\item{modelout}{the name of the output file for the model.
This is the file all the syntax is written to, which becomes the
Mplus input file. It should end in .inp.  If missing, defaults to
\code{dataout} changing the extension to .inp.}

\item{run}{an integer indicating how many models should be run. Defaults to zero.
If zero, the data and model input files are all created, but the model is not run.
This can be useful for seeing how the function works and what setup is done. If one, a basic
model is run. If greater than one, the model is bootstrapped with \code{run} replications as
well as the basic model.}

\item{check}{logical whether the body of the Mplus syntax should be checked for missing
semicolons using the \code{\link{parseMplus}} function. Defaults to \code{FALSE}.}

\item{varwarnings}{A logical whether warnings about variable length should be left, the
default, or removed from the output file.}

\item{Mplus_command}{optional. N.B.: No need to pass this parameter for most users (has intelligent
defaults). Allows the user to specify the name/path of the Mplus executable to be used for
running models. This covers situations where Mplus is not in the system's path,
or where one wants to test different versions of the Mplus program.}

\item{writeData}{A character vector, one of \sQuote{ifmissing},
\sQuote{always}, \sQuote{never} indicating whether the data files
(*.dat) should be written to disk.  This is passed on to \code{prepareMplusData}.
Note that previously, \code{mplusModeler} always (re)wrote the data to disk.
However, now the default is to write the data to disk only if it is missing
(i.e., \sQuote{ifmissing}).  See details for further information.}

\item{hashfilename}{A logical whether or not to add a hash of the raw data to the
data file name.  Defaults to \code{TRUE} in \code{mplusModeler}.  Note that this
behavior is a change from previous versions and differs from \code{prepareMplusData}
which maintains the old behavior by default of \code{FALSE}.}

\item{killOnFail}{A logical whether or not to kill any mplus processes on failure.
Passed on to control behavior of \code{\link{runModels}}. Defaults to \code{TRUE}.}

\item{quiet}{optional. If \code{TRUE}, show status messages in the console.}

\item{\ldots}{additional arguments passed to the
\code{\link[MplusAutomation]{prepareMplusData}} function.}
}
\value{
An Mplus model object, with results.
  If \code{run = 1}, returns an invisible list of results from the run of
  the Mplus model (see \code{\link[MplusAutomation]{readModels}} from the
  MplusAutomation package). If \code{run = 0}, the function returns a list
  with two elements, \sQuote{model} and \sQuote{boot} that are both \code{NULL}.
  if \code{run >= 1},returns a list with two elements, \sQuote{model} and \sQuote{boot}
  containing the regular Mplus model output and the boot object, respectively.
  In all cases, the Mplus data file and input files are created.
}
\description{
This is a convenience wrapper to automate many of the
usual steps required to run an Mplus model. It relies in part
on functions from the MplusAutomation package.
}
\details{
Combined with functions from the MplusAutomation package,
this function is designed to make it easy to fit Mplus models
from R and to ease many of the usual frustrations with Mplus.
For example, Mplus has very specific formats it accepts data in,
but also very little data management facilities. Using \R data
management is easy. This function is designed to make using data
from \R in Mplus models easy.
It is also common to want to fit many different models that are
slight variants. This can be tedius in Mplus, but using \R you can
create one basic set of input, store it in a vector, and then just
modify that (e.g., using regular expressions) and pass it to Mplus.
You can even use loops or the \code{*apply} constructs to fit the same
sort of model with little variants.

The \code{writeData} argument is new and can be used to reduce overhead
from repeatedly writing the same data from R to the disk.  When using the
\sQuote{always} option, \code{mplusModeler} behaves as before, always writing
data from R to the disk.  This remains the default for the \code{prepareMplusData}
function to avoid confusion or breaking old code.  However, for \code{mplusModeler},
the default has been set to \sQuote{ifmissing}.  In this case, R generates an
md5 hash of the data prior to writing it out to the disk.  The md5 hash is based on:
(1) the dimensions of the dataset, (2) the variable names,
(3) the class of every variable, and (4) the raw data from the first and last rows.
This combination ensures that under most all circumstances, if the data changes,
the hash will change.  The hash is appended to the specified data file name
(which is controlled by the logical \code{hashfilename} argument).  Next R
checks in the directory where the data would normally be written.  If a data file
exists in that directory that matches the hash generated from the data, R will
use that existing data file instead of writing out the data again.
A final option is \sQuote{never}.  If this option is used, R will not write
the data out even if no file matching the hash is found.
}
\examples{
\dontrun{
# minimal example of a model using builtin data, allowing R
# to automatically guess the correct variables to use
test <- mplusObject(MODEL = "mpg ON wt hp;
  wt WITH hp;", rdata = mtcars)

 # estimate the model in Mplus and read results back into R
 res <- mplusModeler(test, modelout = "model1.inp", run = 1L)

 # when forcing writeData = "always" data gets overwritten (with a warning)
 resb <- mplusModeler(test, modelout = "model1.inp", run = 1L,
                      writeData = "always")

 # using writeData = "ifmissing", the default, no data re-written
 resc <- mplusModeler(test, modelout = "model1.inp", run = 1L)

 # using writeData = "ifmissing", the default, data ARE written
 # if data changes
 test <- mplusObject(MODEL = "mpg ON wt hp;
   wt WITH hp;", rdata = mtcars[-10, ])
 resd <- mplusModeler(test, modelout = "model1.inp", run = 1L)

 # show summary
 summary(resd)

 # show coefficients
 coef(resd)

 # what if you wanted confidence intervals
 # and standardized values?
 # first update to tell Mplus you want them, re-run and print
 test <- update(test, OUTPUT = ~ "CINTERVAL; STDYX;")
 resd <- mplusModeler(test, modelout = "model1.inp", run = 1L)

coef(resd)
confint(resd)

# now standardized
coef(resd, type = "stdyx")
confint(resd, type = "stdyx")

# put together in one data frame if desired
merge(
  coef(resd, type = "stdyx"),
  confint(resd, type = "stdyx"),
  by = "Label")

 # remove files
 unlink(resc$results$input$data$file)
 unlink(resd$results$input$data$file)
 unlink("model1.inp")
 unlink("model1.out")

# simple example of a model using builtin data
# demonstrates use with a few more sections
test2 <- mplusObject(
  TITLE = "test the MplusAutomation Package and mplusModeler wrapper;",
  MODEL = "
    mpg ON wt hp;
    wt WITH hp;",
  usevariables = c("mpg", "wt", "hp"),
  rdata = mtcars)

 res2 <- mplusModeler(test2, modelout = "model2.inp", run = 1L)

 # remove files
 unlink(res2$results$input$data$file)
 unlink("model2.inp")
 unlink("model2.out")

 # similar example using a robust estimator for standard errors
 # and showing how an existing model can be easily updated and reused
 test3 <- update(test2, ANALYSIS = ~ "ESTIMATOR = MLR;")

 res3 <- mplusModeler(test3, modelout = "model3.inp", run = 1L)
 unlink(res3$results$input$data$file)
 unlink("model3.inp")
 unlink("model3.out")

 # now use the built in bootstrapping methods
 # note that these work, even when Mplus will not bootstrap
 # also note how categorical variables and weights are declared
 # in particular, the usevariables for Mplus must be specified
 # because mroe variables are included in the data than are in the
 # model. Note the R usevariables includes all variables for both
 # model and weights. The same is true for clustering.
 test4 <- mplusObject(
   TITLE = "test bootstrapping;",
   VARIABLE = "
     CATEGORICAL = cyl;
     WEIGHT = wt;
     USEVARIABLES = cyl mpg;",
   ANALYSIS = "ESTIMATOR = MLR;",
   MODEL = "
     cyl ON mpg;",
   usevariables = c("mpg", "wt", "cyl"),
   rdata = mtcars)

 res4 <- mplusModeler(test4, "mtcars.dat", modelout = "model4.inp", run = 10L,
   hashfilename = FALSE)
 # see the results
 res4$results$boot

 # remove files
 unlink("mtcars.dat")
 unlink("model4.inp")
 unlink("model4.out")

# Monte Carlo Simulation Example
montecarlo <- mplusObject(
 TITLE = "Monte Carlo Example;",
 MONTECARLO = "
  NAMES ARE i1-i5;
  NOBSERVATIONS = 100;
  NREPS = 100;
  SEED = 1234;",
 MODELPOPULATION = "
  f BY i1-i5*1;
  f@1;
  i1-i5*1;",
 ANALYSIS = "
  ESTIMATOR = BAYES;
  PROC = 2;
  fbiter = 100;",
 MODEL = "
  f BY i1-i5*.8 (l1-l5);
  f@1;
  i1-i5*1;",
 MODELPRIORS = "
   l1-l5 ~ N(.5 .1);",
 OUTPUT = "TECH9;")

fitMonteCarlo <- mplusModeler(montecarlo,
  modelout = "montecarlo.inp",
  run = 1L,
  writeData = "always",
  hashfilename = FALSE)

unlink("montecarlo.inp")
unlink("montecarlo.out")


# Example including ID variable and extracting factor scores
dat <- mtcars
dat$UID <- 1:nrow(mtcars)

testIDs <- mplusObject(
  TITLE = "test the mplusModeler wrapper with IDs;",
  VARIABLE = "IDVARIABLE = UID;",
  MODEL = "
    F BY mpg wt hp;",
  SAVEDATA = "
    FILE IS testid_fscores.dat;
    SAVE IS fscores;
    FORMAT IS free;",
  usevariables = c("UID", "mpg", "wt", "hp"),
  rdata = dat)

 resIDs <- mplusModeler(testIDs, modelout = "testid.inp", run = 1L)

# view the saved data from Mplus, including factor scores
# the indicator variables, and the ID variable we specified
head(resIDs$results$savedata)

# merge the factor scores with the rest of the original data
# merge together by the ID column
dat <- merge(dat, resIDs$results$savedata[, c("F", "UID")],
  by = "UID")

# correlate merged factor scores against some other new variable
with(dat, cor(F, qsec))




# can write multiply imputed data too
# here are three "imputed" datasets
idat <- list(
  data.frame(mpg = mtcars$mpg, hp = c(100, mtcars$hp[-1])),
  data.frame(mpg = mtcars$mpg, hp = c(110, mtcars$hp[-1])),
  data.frame(mpg = mtcars$mpg, hp = c(120, mtcars$hp[-1])))

# if we turn on hashing in the filename the first time,
# we can avoid overwriting notes the second time
testobjimp <- mplusObject(MODEL = "[mpg];", rdata = idat, imputed = TRUE)

testimp <- mplusModeler(
  testobjimp,
  modelout = "testimp.inp",
  writeData = "ifmissing", hashfilename=FALSE)

testimp <- mplusModeler(
  testobjimp,
  modelout = "testimp.inp",
  writeData = "ifmissing", hashfilename=TRUE)

testimp <- mplusModeler(
  testobjimp,
  modelout = "testimp.inp",
  writeData = "ifmissing", hashfilename=TRUE,
  run = TRUE)

testobjimp2 <- mplusObject(MODEL = "[hp];", rdata = idat, imputed = TRUE)
testimp2 <- mplusModeler(
  testobjimp2,
  modelout = "testimp2.inp",
  writeData = "ifmissing", hashfilename=TRUE,
  run = TRUE)

 # remove files
 unlink(resIDs$results$input$data$file)
 unlink("testid.inp")
 unlink("testid.out")
 unlink("testid_fscores.dat")
 unlink("Mplus Run Models.log")
}
}
\seealso{
\code{\link{runModels}} and \code{\link{readModels}}
}
\author{
Joshua F. Wiley <jwiley.psych@gmail.com>
}
