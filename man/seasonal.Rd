% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/seasonal.R
\name{seasonal}
\alias{seasonal}
\title{Finding the seasonal trend in historical temperatures}
\usage{
seasonal(temp)
}
\arguments{
\item{temp}{A n by 1 vector containing the past temperatures data at one station over (n / 365) years}
}
\value{
A list containing:
\item{a}{a scalar that is the value of the coefficient a in the seasonal function}
\item{b}{a scalar that is the value of the coefficient b in the seasonal function}
\item{c}{a scalar that is the value of the coefficient c in the seasonal function}
\item{d}{a scalar that is the value of the coefficient d in the seasonal function}
\item{seasonality}{n by 1 vector containing the values of the seasonal function}
\item{plt}{plot of the temperatures and the seasonal function over the time indices}
}
\description{
Function that returns the estimated coefficients of the seasonality function
}
\examples{
# load the temp0 data from temp0.rda in the data folder

# example 1
temp = as.numeric(temp0[, 3]) # historical temperatures at one station
seasonal1 = seasonal(temp)

# examine results
c(seasonal1$a, seasonal1$b, seasonal1$c, seasonal1$d) # coefficients
head(seasonal1$seasonality) # fitted values of the seasonal function
seasonal1$plt # plot

# example 2
temp = as.numeric(temp0[, 50]) # historical temperatures at one station
seasonal2 = seasonal(temp)

# examine results
c(seasonal2$a, seasonal2$b, seasonal2$c, seasonal2$d) # coefficients
head(seasonal2$seasonality) # fitted values of the seasonal function
seasonal2$plt # plot
}
