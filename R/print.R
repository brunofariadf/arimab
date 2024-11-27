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

# print for list.timeserieb
#' @export

print.list.timeserieb <- function(object) {
    print(unclass(object))
    return(invisible(unclass(object)))
}

# print for list.acf
#' @export

print.list.acfb <- function(object) {
    print(unclass(object))
    return(invisible(unclass(object)))
}

# print for list.pacf
#' @export

print.list.pacfb <- function(object) {
    print(unclass(object))
    return(invisible(unclass(object)))
}

# print for list.ccf
#' @export

print.list.ccfb <- function(object) {
    print(unclass(object))
    return(invisible(unclass(object)))
}

# print diagnostic
#' @export

print.arimab.univariateb <- function(object, ...) {
    r_i <- object[["d"]][[3]]
    d_i <- unique(object[["d"]][["d"]])
    cat("Call:\n",
        deparse(object[["call"]], width.cutoff = 75L),"\n",
        "exploratory data analysis for time series with ACF and PACF",
        "\n\n Serie\n")

    print.default(
        c(d = d_i, .describe(r_i)),
        print.gap = 2)
    cat("\n")
}

# print diagnostic
#' @export

print.arimab.covariableb <- function(object) {
    r_i <- object[["residual"]]
    cat("Call:\n",
        deparse(object[["call"]], width.cutoff = 75L),"\n",
        "linear regression residual analysis for ACF and PACF",
        "\n\n Residuals\n")

    print.default(.describe(r_i), print.gap = 2)
    cat("\n")
}

# print estimate
#' @export

print.arimab.model <- function(object, digits = 4, se = TRUE, ...) {
    cat("Call:",
        deparse(object[["call"]], width.cutoff = 75L),
        .select_message_arimab(object),
        "", sep = "\n")

    if (length(object[["coef"]])) {
        cat("Coefficients:\n")
        coef <- round(object[["coef"]], digits = digits)
        if (se && NROW(object[["var.coef"]])) {
            ses <- rep.int(0, length(coef))
            ses[object[["mask"]]] <- round(sqrt(diag(object[["var.coef"]])), digits = digits)
            coef <- matrix(coef, 1L, dimnames = list(NULL, names(coef)))
            coef <- rbind(coef, s.e. = ses)
        }
        print.default(coef, print.gap = 2)
    }
    cm <- object[["call"]][["method"]]
    if (is.null(cm) || cm != "CSS") 
        cat("\nsigma^2 estimated as ", format(object[["sigma2"]], 
            digits = digits), ":  log likelihood = ", format(round(object[["loglik"]], 
            2L)), ",  aic = ", format(round(object[["aic"]], 2L)), 
            "\n", sep = "")
    else cat("\nsigma^2 estimated as ", format(object[["sigma2"]], 
        digits = digits), ":  part log likelihood = ", 
        format(round(object[["loglik"]], 2)), "\n", sep = "")
    invisible(object)
}

# print summary
#' @export

print.summary.arimab.model <- function(object, digits = 4, ...) {
    cat("Call:\n",
        deparse(object[["call"]], width.cutoff = 75L),"\n",
        .select_message_arimab(object), "\n",
        "\nResiduals:\n", sep = "")

    r_i <- object[["residual"]]
    print.default(.describe(r_i), print.gap = 2)
    cat("\nCoefficients:\n", sep = "")
    printCoefmat(coef(object), digits)

    cm <- object[["call"]][["method"]]
    if (is.null(cm) || cm != "CSS") 
        cat("\nsigma^2 estimated as ", format(object[["sigma2"]], 
            digits = digits), ":  log likelihood = ", format(round(object[["loglik"]], 
            2L)), ",  aic = ", format(round(object[["aic"]], 2L)), 
            "\n", sep = "")
    else cat("\nsigma^2 estimated as ", format(object[["sigma2"]], 
        digits = digits), ":  part log likelihood = ", 
        format(round(object[["loglik"]], 2)), "\n", sep = "")
    invisible(object)
}

# print time serie
#' @export

print.arimab.timeserieb <- function(object, ...) {
    cat("\n",
        "exploratory data analysis", "\n",
        "Description of time series from ", 
        sQuote(object[["vars.names"]]), ", in ", 
        sQuote(object[["data.names"]]), "\n", sep = "")
    invisible(object)
}

# print acf
#' @export

print.arimab.acfb <- function(object, ...) {
    type_i <- match(object[["type"]], c("correlation", "covariance", "partial"))
    msg_i <- c("Autocorrelations", "Autocovariances", "Partial autocorrelations")
    cat("\n",
        .select_message_acfb(object), "\n",
        msg_i[type_i], " of series ", sQuote(object[["vars.names"]]),
        ", by lag\n", sep = "")
    invisible(object)
}

# print pacf
#' @export

print.arimab.pacfb <- function(object, ...) {
    type_i <- match(object[["type"]], c("correlation", "covariance", "partial"))
    msg_i <- c("Autocorrelations", "Autocovariances", "Partial autocorrelations")    
    cat("\n",
        .select_message_acfb(object), "\n",
        msg_i[type_i], " of series ", sQuote(object[["vars.names"]]),
        ", by lag\n", sep = "")
    invisible(object)
}

# print ccf
#' @export

print.arimab.ccfb <- function(object, ...) {
    type_i <- match(object[["type"]], c("correlation", "covariance"))
    msg_i <- c("Autocorrelations", "Autocovariances")    
    cat("\n",
        .select_message_acfb(object), "\n",
        msg_i[type_i], " of series ", 
        sQuote(paste0(object[["y"]], " ~ ", 
            object[["vars.names"]])),
        ", by lag\n", sep = "")
    invisible(object)
}

# print diagnostic
#' @export

print.arimab.diagnosticb <- function(object) {
    r_i <- object[["residual"]]
    cat("Call:\n",
        deparse(object[["call"]], width.cutoff = 75L),"\n",
        "diagnostic of adjustment from", .select_message_arimab(object),
        "\n\n Residuals\n")

    print.default(.describe(r_i), print.gap = 2)
    cat("\n")
}

# print predict
#' @export

print.arimab.predict <- function(object, ...) {
    cat("Call:\n",
        deparse(object[["call"]], width.cutoff = 75L),"\n",
        "predict from ",
        .select_message_arimab(object), "\n",
        "\n", sep = "")

    df_i <- object[["accuracy"]]
    row.names(df_i) <- c("fitted", "predict")
    cat("Accuracy:\n")
    print(df_i)
    invisible(object)
}

# print predict
#' @export

print.ggpredict <- function(object) {
    invisible(unclass(object))
}
