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

#' Time Series Description
#'
#' The `timeserieb()` creates a class object 'list.timeserieb' for univariate or multiple time series exploration.
#'
#' @usage timeserieb(formula, data, date = NULL)
#' @param formula an object of class `formula` (or one that can be coerced to that class): a symbolic description of the model to be exploration.
#' @param data a class object of a `data frame`.
#' @param date character class vector that indicates the column name in data frame.
#' @details `timeserieb` adds the Augmented Dickey-Fuller (ADF) hypothesis test to analyze how many differentiations are needed for the time series to be stationary. For `stationary = 0` the time series is stationary without differentiation.
#' @references A. Banerjee, J. J. Dolado, J. W. Galbraith, and D. F. Hendry (1993): \emph{Cointegration, Error Correction, and the Econometric Analysis of Non-Stationary Data}, Oxford University Press, Oxford.
#' 
#' S. E. Said and D. A. Dickey (1984): Testing for Unit Roots in Autoregressive-Moving Average Models of Unknown Order. \emph{Biometrika} \bold{71}, 599--607.
#' @return a class object `list.timeserieb` with time series description.
#' @export 
#' @examples
#' timeb <- arimab::timeserieb(rate ~ ., arimab::bacen, date = "date")
#' plot(timeb)

timeserieb <- function(formula, data, date = NULL) {
    name_data <- deparse(substitute(data))
    .check_timeserie(formula, data, date, name_data)
}

#' Lag a Time Series
#'
#' The `lagb()` moves a time series by shifting the vector in time backwards or forwards in a given number of observations.
#'
#' @usage lagb(x, k)
#' @param x numeric class vector.
#' @param k numeric class vector indicating the number of forward or backward lags.
#' @details `lagb` is a wrapper of the `lag` function with focus is on vector for `data frame`. It is important to note that delays cause missing value in the vector, but in the function with `arimab` the missing values are removed for adjustment.
#' @references Hyndman, R. J., Athanasopoulos, G. (2016) \emph{Forecasting: principles and practice}. <https://otexts.com/fpp2>.
#' @seealso [arimab]
#' @export 
#' @examples
#' library(arimab)
#' 
#' # explory data
#' rate_i <- head(arimab::bacen[["rate"]])
#' rate_i;lagb(rate_i, 0)
#' lagb(rate_i, -2)
#' lagb(rate_i, 2)
#' 
#' # fit model
#' data_i <- arimab::bacen
#' df_train <- data_i[1:100,]
#' fit_arima <- arimab::arimab(rate ~ lagb(inflation, -7), df_train)
#' summary(fit_arima)
#' plot(fit_arima)

lagb <- function(x, k) {
    .check_lagb(x, k)
}

#' Autocorrelation and Cross-Correlation Function Estimation
#'
#' The `acfb()` computes estimates of the autocovariance or autocorrelation function with `acf`, `pacf` and `ccf`. Function `acfb` with group = 'pacf' is the function used for the partial autocorrelations. Function `acfb` with group = 'ccf' computes the cross-correlation or cross-covariance of two time series.
#'
#' @usage acfb(object, data = NULL, group = "acf",
#'      method = "prewhiten", ci = 0.95, ...)
#' acfb(object, data = NULL, group = "pacf")
#' acfb(object, data = NULL, group = "ccf")
#' @param object A class object `formula` or a class object `arimab.model`. For `formula`, an object of class `formula` (or one that can be coerced to that class): a symbolic description of the model to be exploration. For `arimab.model`, a fit model with the function `arimab`.
#' @param data a class object of a `data frame`.
#' @param group character class vector that indicates the estimates of the autocovariance or autocorrelation function.
#' @param method character class vector that indicates the method of transformation in the variables with option of `prewhiten` and `default` with ccf computes the cross-correlation or cross-covariance of two univariate series.
#' @param ci numeric class vector that indicates the confidence interval for `acf`.
#' @param ... further arguments to be passed to `acf`, `pacf` or `ccf`.
#' @details `acfb` is a wrapper of the `acf`, `pacf` and `ccf` function with the aim of simplifying an data explory for univariate and multiple time series. The focus is on fit per ⁠`data frame`⁠. In addition to providing the `formula` object for data explory, it also makes the `list.acfb`, `list.pacfb` and `list.ccfb` object available for plotting.
#' 
#' An additional feature for the `acfb` function is the implementation of the pre-whitening method to analyze the cross-correlation for group = `ccf`. If the time series is stationary, the group = `default` method does not make any transformations to the time series.
#' @return a class object `list.acfb` with time series exploration.
#' @references Venables, W. N. and Ripley, B. D. (2002) \emph{Modern Applied Statistics with S}. Fourth Edition. Springer-Verlag.
#' 
#' Box, G., Jenkins, G. M., Reinsel, G. C. and Ljung, G. M (2015) \emph{Time series analysis: forecasting and control}. Fifth Edition. Wiley.
#' @seealso \code{\link{arimab}}
#' @export 
#' @examples
#' # exploration with data
#' plot(arimab::acfb(rate ~ ., arimab::bacen, group = "acf"))
#' plot(arimab::acfb(rate ~ ., arimab::bacen, group = "pacf"))
#' plot(arimab::acfb(rate ~ ., arimab::bacen, group = "ccf"))
#' 
#' # exploration with arimab
#' fit_arimab <- arimab::arimab(rate ~ inflation, arimab::bacen)
#' plot(arimab::acfb(fit_arimab))
#' plot(arimab::acfb(fit_arimab, group = "ccf"))

acfb <- function(object, data = NULL, lag.max = NULL,
    group = "acf", method = "prewhiten", ci = 0.95, ...) {
    .check_acfb(object, data, lag.max, group, method, ci, ...)
}

#' Autocorrelation and Partial Autocorrelation Function Estimation
#'
#' The `univariateb()` computes estimates of the autocorrelation and partial autocorrelation function with `acf` and `pacf`. Function `acfb` with group = 'pacf' is the function used for the partial autocorrelations to the univariate time series.
#'
#' @usage univariateb(formula, data, d = NULL, ci = 0.95, ...)
#' @param formula an object of class `formula` (or one that can be coerced to that class): a symbolic description of the model to be exploration.s
#' @param data a class object of a `data frame`.
#' @param d a numeric class vector indicating the amount of differentiation for the time series.
#' @param ci a numeric class vector that indicates the confidence interval for `acf`.
#' @param ... further arguments to be passed to `acf`, `pacf` or `ccf`.
#' @details `univariateb` is a wrapper of the `acf` and `pacf` function with the aim of simplifying an data explory for univariate time series.
#' @return a class object `arimab.univariateb` with time series exploration.
#' @references Venables, W. N. and Ripley, B. D. (2002) \emph{Modern Applied Statistics with S}. Fourth Edition. Springer-Verlag.
#' 
#' Box, G., Jenkins, G. M., Reinsel, G. C. and Ljung, G. M (2015) \emph{Time series analysis: forecasting and control}. Fifth Edition. Wiley.
#' @seealso \code{\link{arimab}}
#' @export 
#' @examples
#' # exploration with data
#' plot(arimab::univariateb(rate ~ 1, arimab::bacen))
#' plot(arimab::univariateb(inflation ~ 1, arimab::bacen))
#' plot(arimab::univariateb(inflation ~ 1, arimab::bacen, d = 2))

univariateb <- function(formula, data, d = NULL, ci = 0.95, ...) {
    .check_univariateb(formula, data, d, ci, f = match.call(), ...)
}

#' Estimation of the Autocorrelation Function for the Residual of the Covariate in Linear Regression
#'
#' The `covariableb()` function analyzes the residuals of the covariates of a linear regression by estimating the autocorrelation and partial autocorrelation functions.
#'
#' @usage covariableb(formula, data, ci = 0.95, ...)
#' @param formula an object of class `formula` (or one that can be coerced to that class): a symbolic description of the model to be exploration.s
#' @param data a class object of a `data frame`.
#' @param ci numeric class vector that indicates the confidence interval for `acf`.
#' @param ... further arguments to be passed to `acf` and `pacf`.
#' @details The method of analysis of the residuals by linear regression for the `acf` and `pacf` method only evaluates the estimation of the fit without performing transformation and using the standard argument of the `lm` function as implemented in the textbook.
#' @return a class object `arimab.covariableb` with time series exploration.
#' @references Cryer, J. Chan, K. (2008). \emph{Time Series Analysis: With Applications in R}. Springer New York.
#' @seealso \code{\link{acfb}}, \code{\link{arimab}}
#' @export 
#' @examples
#' 
#' # explory
#' data_i <- arimab::bacen
#' df_train <- data_i[1:100,]
#' plot(acfb(rate ~ ., df_train, group = "ccf"))
#' check_covariable <- arimab::covariableb(rate ~ arimab::lagb(inflation, -7),
#'  data = df_train)
#' 
#' # adjustment
#' plot(check_covariable)

covariableb <- function(formula, data, ci = 0.95, ...) {
    .check_covariableb(formula, data, ci, f = match.call(), ...)
}

#' Fitting for ARIMA time series modeling
#'
#' The `arimab()` is an adjustment with ARIMA and auto.ARIMA for univariate and multiple time series.
#'
#' @usage arimab(formula, data, order = NULL, seasonal = NULL, 
#'        include_mean = FALSE, ...)
#' @param formula character class vector that indicates the message of the title or subtitle of the comment.
#' @param data a class object of a `data frame`.
#' @param order numeric class vector that indicates a specification of the non-seasonal part of the ARIMA model: the three integer components (p,d,q) are the AR order, the degree of differencing, and the MA order.
#' @param seasonal numeric class vector or list that indicates \code{order} and \code{period} to the a specification of the seasonal part of the ARIMA model, plus the period (which defaults to \code{frequency(x)}. This may be a list with components order and period, or just a numeric vector of length 3 which specifies the seasonal order. In the latter case the default period is used.
#' @param include_mean logical class vector that indicates the should the ARMA model include a mean/intercept term? The default is FALSE for time series.
#' @param ... further arguments to be passed to `arima`.
#' @details `arimab` is a wrapper of the `arima` function with the aim of simplifying an adjustment for univariate and multiple time series. The focus is on fit per `data frame`. In addition to providing the `formula` object for model fitting, it also makes the `arimab.model` object available for plotting.
#' 
#' An additional feature for the `arimab` function is the availability of the `auto.arima` method if order = `NULL`. The implementation does not consider seasonality and other characteristics of the time series.
#' @return a class object `arimab.model`.
#' @references Brockwell, P. J. and Davis, R. A. (1996). \emph{Introduction to Time Series and Forecasting}. Springer, New York. Sections 3.3 and 8.3.
#' 
#' Durbin, J. and Koopman, S. J. (2001). \emph{Time Series Analysis by State Space Methods}. Oxford University Press.
#' 
#' Gardner, G, Harvey, A. C. and Phillips, G. D. A. (1980). Algorithm AS 154: An algorithm for exact maximum likelihood estimation of autoregressive-moving average models by means of Kalman filtering. \emph{Applied Statistics}, \bold{29}, 311--322. \ifelse{text}{doi:10.2307/2346910 <https://doi.org/10.2307/2346910>}{\ifelse{latex}{\href{https://doi.org/10.2307/2346910}{doi:10.2307\out{\slash{}}2346910}}{\href{https://doi.org/10.2307/2346910}{doi:10.2307/2346910}}}.
#' 
#' Harvey, A. C. (1993). \emph{Time Series Models}. 2nd Edition. Harvester Wheatsheaf. Sections 3.3 and 4.4.
#' 
#' Jones, R. H. (1980). Maximum likelihood fitting of ARMA models to time series with missing observations. \emph{Technometrics}, \bold{22}, 389--395. \ifelse{text}{doi:10.2307/1268324 <https://doi.org/10.2307/1268324>}{\ifelse{latex}{\href{https://doi.org/10.2307/1268324}{doi:10.2307\out{\slash{}}1268324}}{\href{https://doi.org/10.2307/1268324}{doi:10.2307/1268324}}}.
#' 
#' Ripley, B. D. (2002). \dQuote{Time series in \R 1.5.0}. \emph{R News}, \bold{2}(2), 2--7. \url{https://www.r-project.org/doc/Rnews/Rnews_2002-2.pdf}
#' 
#' Box, G., Jenkins, G. M., Reinsel, G. C. and Ljung, G. M (2015) \emph{Time series analysis: forecasting and control}. Fifth Edition. Wiley.
#' 
#' Hyndman, R. J., Athanasopoulos, G. (2016) \emph{Forecasting: principles and practice}. <https://otexts.com/fpp2>.
#' 
#' Cryer, J. Chan, K. (2008). \emph{Time Series Analysis: With Applications in R}. Springer New York.
#' @seealso \code{\link{lagb}}, \code{\link{acfb}}, \code{\link{diagnosticb}}
#' @export 
#' @examples
#' # adjustment
#' fit_arima <- arimab::arimab(rate ~ ., arimab::bacen)
#' summary(fit_arima)
#' fit_arima <- update(fit_arima, rate ~ log(ibc_br))
#' summary(fit_arima)
#' arimab::diagnosticb(fit_arima)
#' plot(acfb(fit_arima))
#' plot(fit_arima)
#' 
#' # fitted 1
#' fit_arima_predict <- predict(fit_arima)
#' plot(fit_arima_predict)
#' 
#' # fitted 2
#' data_i <- arimab::bacen
#' df_train <- data_i[1:100,]
#' df_test <- data_i[101:113,]
#' fit_arima_uni  <- arimab::arimab(rate ~ 1, df_train)
#' fit_arima_mult <- arimab::arimab(rate ~ arimab::lagb(inflation, -7), df_train)
#' rbind(arimab::accuracy(predict(fit_arima_uni)),
#'      arimab::accuracy(predict(fit_arima_mult)))
#' 
#' # predict
#' select_vars <- arimab::extractb(fit_arima_mult, df_test)
#' fit_arima_predict <- predict(fit_arima_mult, 
#'      h = 4, newdata = select_vars)
#' plot(fit_arima_predict)

arimab <- function(formula, data, 
    order = NULL, seasonal = NULL, include_mean = FALSE, ...) {

    .check_arimab(formula, data, order, seasonal, 
        include_mean, f = match.call(), ...)
}

#' Diagnostics measures from a arimab model
#'
#' The `diagnosticb()` returns the summary of diagnostics measurements from a `arimab.model` object.
#'
#' @usage diagnosticb(object, ci = 0.95)
#' @param object `arimab.model` class object.
#' @param ci numeric class vector that indicates the confidence interval for `acf`.
#' @details The method for arimab object plot residuals scaled by the estimate of their (individual) variance, and use the Ljung–Box version of the portmanteau test.
#' @references Box, G. E. P. and Pierce, D. A. (1970), Distribution of residual correlations in autoregressive-integrated moving average time series models. \emph{Journal of the American Statistical Association}, \bold{65}, 1509--1526.
#' 
#' Ljung, G. M. and Box, G. E. P. (1978), On a measure of lack of fit in time series models. \emph{Biometrika} \bold{65}, 297--303.
#' 
#' Harvey, A. C. (1993) \emph{Time Series Models}. 2nd Edition, Harvester Wheatsheaf, NY, pp.\sspace{}44, 45.
#' @seealso \code{\link{arimab}}
#' @export 
#' @examples
#' data_i <- arimab::bacen
#' df_train <- data_i[1:100,]
#' fit_arima <- arimab::arimab(rate ~ arimab::lagb(inflation, -7),
#'  data = df_train)
#' arimab::diagnosticb(fit_arima)
#' plot(arimab::diagnosticb(fit_arima))

diagnosticb <- function(object, ci = 0.95) {
    name_obj <- deparse(substitute(object))
    .check_diagnosticb(object, name_obj, ci, f = match.call())
}

#' Extract Dataset from Fitted Model
#'
#' The `extractb()` is a tool for the `predict` function where `newdata != NULL` and serves to select variables from a test dataset from the model fitted in the training dataset.
#'
#' @usage extractb(object, newdata)
#' @param object `arimab.model` class object.
#' @param newdata data frame for the test data.
#' @details The `extractb` function will be useful when there is transformation in the covariates for the fit with `arimab`. If the fit with `arimab` has past and future lags far above the number of observations in the test data frame, then it is possible that the output of the function is empty since it only generated missing values for the test data due to the transformations of the fit with `arimab`.
#' @seealso \code{\link{predict}}
#' @export 
#' @examples
#' # data
#' data_i <- arimab::bacen
#' df_train <- data_i[1:100,]
#' df_test <- data_i[101:113,]
#' 
#' # model
#' fit_arima <- arimab::arimab(rate ~ arimab::lagb(inflation, -7),
#'  data = df_train)
#' df_fit <- fit_arima[["data"]][["data"]]
#' df_newdata <- arimab::extractb(fit_arima, df_test)
#' colnames(df_fit) == colnames(df_newdata)
#' colnames(df_fit);colnames(df_newdata)
#' 
#' # predict
#' plot(predict(fit_arima, h = 3, newdata = df_newdata))

extractb <- function(object, newdata) {
    .check_extractb(object, newdata)
}

#' Accuracy measures from arimab model
#'
#' The `accuracy()` returns the summary of accuracy measurements from a `arimab.predict` object.
#'
#' @usage accuracy(object)
#' @param object `arimab.predict` class object.
#' @references Hyndman, R. J., Athanasopoulos, G. (2016) \emph{Forecasting: principles and practice}. <https://otexts.com/fpp2>.
#' @export 
#' @examples
#' # data
#' data_i <- arimab::bacen
#' df_train <- data_i[1:100,]
#' df_test <- data_i[101:113,]
#' 
#' # model
#' fit_arima <- arimab::arimab(rate ~ arimab::lagb(inflation, -7),
#'  data = df_train)
#' df_test <- arimab::extractb(fit_arima, df_test)
#' pred_arima <- predict(fit_arima, df_test, h = 3)
#' arimab::accuracy(pred_arima)
#' #    n           me         mse      rmse       mae
#' # 1 93  0.009170212 0.007820629 0.1876783 0.1371477
#' # 2  3 -0.268580465 0.216406398 0.3284197 0.2685805

accuracy <- function(object) {
    .check_accuracy(object)
}
