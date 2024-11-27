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

# acf
# not export

.acf <- function(object, data, ci, ...) {
    .acf_i <- function(object, x, ...) {
        acf_i <- acf(x, plot = FALSE, ...)
        ic_i <- .ic_acf(acf_i, ci)
        acf_i <- list(
            acf = as.vector(acf_i[["acf"]]), ci = ic_i,
            type = acf_i[["type"]], group = "acf",
            n.used = as.vector(acf_i[["n.used"]]), 
            lag = as.vector(acf_i[["lag"]]))
        acf_i[["y"]] <- .select_acf_y(object, data)
        acf_i[["vars.names"]] <- "x"
        acf_i[["fit"]] <- .select_model_name(object)
        acf_i[["order"]] <- object[["order"]]
        acf_i[["data"]] <- object[["data"]]
        class(acf_i) <- c("arimab.acfb")
        return(acf_i)
    }

    if (.is_arimab(object)) {
        df_i <- .select_data(object, data)
        lapply(df_i, function(x).acf_i(object, x, ...))
    } else if (.is_formula(object)) {
        # df_i <- .select_data(object, data)[["data"]]
        df_i <- .select_numeric(data)
        lapply(df_i, function(x).acf_i(object, x, ...))
    }
}

# pacf
# not export

.pacf <- function(object, data, ci, ...) {
    .pacf_i <- function(object, x, ...) {
        pacf_i <- pacf(x, plot = FALSE, ...)
        ic_i <- .ic_acf(pacf_i, ci)
        pacf_i <- list(
            acf = as.vector(pacf_i[["acf"]]), ci = ic_i,
            type = pacf_i[["type"]], group = "pacf", 
            n.used = as.vector(pacf_i[["n.used"]]), 
            lag = as.vector(pacf_i[["lag"]]))
        pacf_i[["y"]] <- .select_acf_y(object, data)
        pacf_i[["vars.names"]] <- "x"
        pacf_i[["fit"]] <- .select_model_name(object)
        pacf_i[["order"]] <- object[["order"]]
        pacf_i[["data"]] <- object[["data"]]
        class(pacf_i) <- c("arimab.pacfb")
        return(pacf_i)
    }

    if (.is_arimab(object)) {
        df_i <- .select_data(object, data)
        lapply(df_i, function(x).pacf_i(object, x, ...))
    } else if (.is_formula(object)) {
        # df_i <- .select_data(object, data)[["data"]]
        df_i <- .select_numeric(data)
        lapply(df_i, function(x).pacf_i(object, x, ...))
    }
}

# ccf
# not export

.ccf <- function(object, data, lag.max, ci, method, ...) {
    .ccf_i <- function(object, x, y, lag.max, method, ...) {
        t_i <- .method_xy(x, y, method)
        x_i <- t_i[["x"]]
        y_i <- t_i[["y"]]
        ccf_i <- ccf(x_i, y_i, lag.max,
            na.action = na.pass, plot = FALSE, ...)
        ic_i <- .ic_acf(ccf_i, ci)
        ccf_i <- list(
            acf = as.vector(ccf_i[["acf"]]), ci = ic_i,
            type = ccf_i[["type"]], group = "ccf",
            n.used = as.vector(ccf_i[["n.used"]]),
            lag = as.vector(ccf_i[["lag"]]))
        ccf_i[["y"]] <- .select_acf_y(object, data)
        ccf_i[["vars.names"]] <- "x"
        ccf_i[["fit"]] <- .select_model_name(object)
        ccf_i[["order"]] <- object[["order"]]
        ccf_i[["data"]] <- object[["data"]]
        class(ccf_i) <- c("arimab.ccfb")
        return(ccf_i)
    }

    if (.is_arimab(object)) {
        if (.is_univariate(object)) {
            stop("Method applicated to the 'arimab' model multiple.")
        } else if (.is_multiple(object)) {
            df_i <- .select_data(object)
            name_y <- object[["vars.names"]][["y"]]
            lapply(df_i, function(x).ccf_i(object, x, 
                df_i[[name_y]], lag.max, method, ...))
        }
    } else if (.is_formula(object)) {
        # l_i <- .select_data(object, data)
        # df_i <- l_i[["data"]]
        df_i <- .select_numeric(data)
        name_y <- .get_variable(object, data)[["y"]]
        lapply(df_i, function(x).ccf_i(object, x, 
            df_i[[name_y]], lag.max, method, ...))
    }
}

# confidence interval to the correlation
# not export

.ic_acf <- function(object, ci) {
    obj_i <- object
    n_i <- object[["n.used"]]
    est_i <- (1 + ci)/2
    ic_i <- c(-1,1)*qnorm(est_i)/sqrt(n_i)
    list(ci = ci, interval = ic_i)
}

# p-value to the ljung-Box
# not export

.pvalue <- function(x, lag) {
    .ljungbox <- function(x, lag) {
        Box.test(x, lag, type = "Ljung-Box")[["p.value"]]
    }

    as.vector(unlist(lapply(lag,
        function(i).ljungbox(x, i))))
}
