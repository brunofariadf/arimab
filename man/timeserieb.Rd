% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main.R
\name{timeserieb}
\alias{timeserieb}
\title{Time Series Description}
\usage{
timeserieb(formula, data, date = NULL)
}
\arguments{
\item{formula}{an object of class \code{formula} (or one that can be coerced to that class): a symbolic description of the model to be exploration.}

\item{data}{a class object of a \verb{data frame}.}

\item{date}{character class vector that indicates the column name in data frame.}
}
\value{
a class object \code{list.timeserieb} with time series description.
}
\description{
The \code{timeserieb()} creates a class object 'list.timeserieb' for univariate or multiple time series exploration.
}
\details{
\code{timeserieb} adds the Augmented Dickey-Fuller (ADF) hypothesis test to analyze how many differentiations are needed for the time series to be stationary. For \code{stationary = 0} the time series is stationary without differentiation.
}
\examples{
timeb <- arimab::timeserieb(rate ~ ., arimab::bacen, date = "date")
plot(timeb)
}
\references{
A. Banerjee, J. J. Dolado, J. W. Galbraith, and D. F. Hendry (1993): \emph{Cointegration, Error Correction, and the Econometric Analysis of Non-Stationary Data}, Oxford University Press, Oxford.

S. E. Said and D. A. Dickey (1984): Testing for Unit Roots in Autoregressive-Moving Average Models of Unknown Order. \emph{Biometrika} \bold{71}, 599--607.
}
