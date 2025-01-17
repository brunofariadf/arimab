% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main.R
\name{covariableb}
\alias{covariableb}
\title{Estimation of the Autocorrelation Function for the Residual of the Covariate in Linear Regression}
\usage{
covariableb(formula, data, ci = 0.95, ...)
}
\arguments{
\item{formula}{an object of class \code{formula} (or one that can be coerced to that class): a symbolic description of the model to be exploration.s}

\item{data}{a class object of a \verb{data frame}.}

\item{ci}{numeric class vector that indicates the confidence interval for \code{acf}.}

\item{...}{further arguments to be passed to \code{acf} and \code{pacf}.}
}
\value{
a class object \code{arimab.covariableb} with time series exploration.
}
\description{
The \code{covariableb()} function analyzes the residuals of the covariates of a linear regression by estimating the autocorrelation and partial autocorrelation functions.
}
\details{
The method of analysis of the residuals by linear regression for the \code{acf} and \code{pacf} method only evaluates the estimation of the fit without performing transformation and using the standard argument of the \code{lm} function as implemented in the textbook.
}
\examples{

# explory
data_i <- arimab::bacen
df_train <- data_i[1:100,]
plot(acfb(rate ~ ., df_train, group = "ccf"))
check_covariable <- arimab::covariableb(rate ~ arimab::lagb(inflation, -7),
 data = df_train)

# adjustment
plot(check_covariable)
}
\references{
Cryer, J. Chan, K. (2008). \emph{Time Series Analysis: With Applications in R}. Springer New York.
}
\seealso{
\code{\link{acfb}}, \code{\link{arimab}}
}
