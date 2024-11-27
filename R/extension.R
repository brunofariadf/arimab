## ------------------------------------------
##                          Date: 2024-11-04
## ------------------------------------------
## Project: arimab
## Author: Bruno Faria
## 
## GUI: VSCode (1.95.1)
## Language: R (4.3.1)
## Platform: Windows
## ------------------------------------------

# print N
#' @importFrom stats nobs
#' @export

nobs.arimab.model <- function(object, ...) {
    object[["nobs"]]
}

# print coefficients
#' @importFrom stats coef
#' @export

coef.arimab.model <- function(object) {
    object[["coef"]]
}

#' @export

coef.summary.arimab.model <- function(object) {
    object[["coefficient"]]
}

#' @importFrom stats coefficients
#' @export

coefficients.arimab.model <- function(object) {
    object[["coef"]]
}

#' @export

coefficients.summary.arimab.model <- function(object) {
    object[["coefficient"]]
}

#' @importFrom stats model.frame
#' @export

model.frame.arimab.model <- function(object) {
    object[["data"]][["data"]]
}

#' @export

model.frame.arimab.predict <- function(object) {
    object[["data"]][["data"]]
}

# print summary
#' @export

summary.arimab.model <- function(object, ...) {
    .get_summary_arima(object, ...)
}

# print variance and covariance
#' @importFrom stats vcov
#' @export

vcov.arimab.model <- function (object) {
    object[["var.coef"]]
}

# print variance and covariance
#' @importFrom stats logLik
#' @export

logLik.arimab.model <- function (object) {
    res <- if(is.na(object[["aic"]])) NA
    else structure(object[["loglik"]], df = sum(object[["mask"]]) + 1, nobs = object[["nobs"]])
    class(res) <- "logLik"
    res
}

# print residuals
#' @importFrom stats residuals
#' @export

residuals.arimab.model <- function(object) {
    object[["residuals"]]
}

#' @importFrom stats resid
#' @export

resid.arimab.model <- function(object) {
    object[["residuals"]]
}

#' @importFrom stats rstandard
#' @export

rstandard.arimab.model <- function(object) {
    r_i <- residuals(object)
    sigma_i <- object[["sigma2"]]
    as.vector(r_i/sqrt(sigma_i))
}

# print fitted
#' @importFrom stats fitted
#' @export

fitted.arimab.model <- function(object) {
  object[["fitted"]]
}

# print terms
#' @importFrom stats terms
#' @export

terms.arimab.model <- function(object) {
  object[["terms"]]
}

# print terms
#' @export

terms.summary.arimab.model <- function(object) {
    object[["terms"]]
}

# print extract aic
#' @importFrom stats extractAIC
#' @export

extractAIC.arimab.model <- function (object, scale = 0, k = 2, ...) {
    n_i <- length(residuals(object))
    name_x <- object[["vars.names"]][["x"]]
    df.i <- sum(object[["order"]]) + length(name_x)
    edf_i <- n_i - df.i
    aic_i <- AIC(object)
    c(edf_i, aic_i+(k-2)*edf_i)
}

# predict arima
#' @importFrom stats predict
#' @export

predict.arimab.model <- function(object, ...) {
    .check_predict_arima(object, ...)
}

# print plot for time serie
#' @export

plot.list.timeserieb <- function(object, ...) {
    ggtimeserie(object, ...)
}

# print plot for acf
#' @export

plot.list.acfb <- function(object, ...) {
    ggacfb(object, ...)
}

# print plot for pacf
#' @export

plot.list.pacfb <- function(object, ...) {
    ggacfb(object, ...)
}

# print plot for ccf
#' @export

plot.list.ccfb <- function(object, ...) {
    ggacfb(object, ...)
}

# print plot for univariate
#' @export

plot.arimab.univariateb <- function(object, ...) {
    ggunivariateb(object, ...)
}

# print plot for lm residual
#' @export

plot.arimab.covariableb <- function(object, ...) {
    ggcovariableb(object, ...)
}

# print plot for diagnostic
#' @export

plot.arimab.model <- function(object, ...) {
    ggdiagnosticb(diagnosticb(object), ...)
}

# print plot for diagnostic
#' @export

plot.arimab.diagnosticb <- function(object, ...) {
    ggdiagnosticb(object, ...)
}

# print plot for diagnostic
#' @export

plot.arimab.predict <- function(object, ...) {
    suppressWarnings(print(ggpredict(object, ...)))
}

# importFrom("stats", "AIC", "BIC", "Box.test", "KalmanForecast", "acf",
#             "ar", "arima", "as.formula", "as.ts", "ccf", "deltat",
#             "filter", "frequency", "lag", "lm", "median", "na.omit",
#             "na.pass", "pacf", "printCoefmat", "pt", "qnorm", "time",
#             "ts", "tsp")
# importFrom("utils", "capture.output", "head", "tail")