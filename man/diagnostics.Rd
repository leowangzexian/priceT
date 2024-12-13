% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/diagnostics.R
\name{diagnostics}
\alias{diagnostics}
\title{Descriptive analysis of deseasonalized temperatures data}
\usage{
diagnostics(resid)
}
\arguments{
\item{resid}{A n by 1 vector containing the past deseasonalized temperatures data at one station over (n / 365) years}
}
\value{
A list containing:
\item{teststat}{a scalar that is the test statistic from the Kolmogorov-Smirnov normality test}
\item{pvalue}{a scalar that is the p-value from the Kolmogorov-Smirnov normality test}
\item{plt1}{QQ plot against normal distribution}
\item{plt2}{QQ plot against generalised hyperbolic distribution}
\item{plt3}{a plot for the kernel density estimate}
}
\description{
Function used for simple preliminary tests on the deseasonalized temperatures data,
returns test statistics and p-values from tests such as the Kolmogorov-Smirnov test,
and visual outputs such as Q-Q plots and kernel density estimates
}
\examples{
# load the residuals data from residuals.rda in the data folder

# example 1
resid = as.numeric(residuals[, 3]) # deseasonalized temperaures at one station
diag1 = diagnostics(resid)

# examine results
diag1$teststat # test statistic from the Kolmogorov-Smirnov normality test
diag1$pvalue # p-value from the Kolmogorov-Smirnov normality test
# smaller p-value, especially close to 0, indicates rejection of the normality assumption
diag1$plt1 # QQ plot against normal
diag1$plt2 # QQ plot against generalised hyperbolic
diag1$plt3 # kernel density estimate

# example 2
resid = as.numeric(residuals[, 53]) # deseasonalized temperaures at one station
diag2 = diagnostics(resid)

# examine results
diag2$teststat # test statistic from the Kolmogorov-Smirnov normality test
diag2$pvalue # p-value from the Kolmogorov-Smirnov normality test
# smaller p-value, especially close to 0, indicates rejection of the normality assumption
diag2$plt1 # QQ plot against normal
diag2$plt2 # QQ plot against generalised hyperbolic
diag2$plt3 # kernel density estimate
}
