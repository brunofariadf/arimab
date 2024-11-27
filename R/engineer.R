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

# time serie explory
#' @importFrom stats AIC BIC Box.test KalmanForecast acf ar arima as.formula as.ts 
#' ccf deltat filter frequency lag lm median na.omit na.pass pacf printCoefmat 
#' pt qnorm time ts tsp
#' @importFrom utils capture.output head tail
# not export

.get_timeserie <- function(formula, data, date, name_data) {
    .all_timeserie <- function(formula, data, date) {
        .timeserie <- function(x, data, date) {
            name_data_i <- deparse(substitute(data))
            n_i <- nrow(data)
            station_i <- tryCatch(ndiference(x),
                error=function(i)"NA")
            freq_i <- frequency(x)
            df_i <- data.frame(
                date = .date(data, date), y = x, 
                check.names = FALSE)
            date_name <- if (is.null(date)) "time" else date
            colnames(df_i) <- c(date_name, "y")
            list_i <- list(
                vars.names = "x",
                data.names = name_data,
                date.names = date_name,
                stationary = station_i,
                frequency = freq_i,
                data = df_i)
            class(list_i) <- "arimab.timeserieb"
            return(list_i)
        }

        .date <- function(data, date) {
            if (is.null(date)) {
                1:nrow(data)
            } else {
                data[[date]]
            }
        }

        # df_i <- .select_data(formula, data)[["data"]]
        df_i <- .select_numeric(data)
        lapply(df_i, function(x).timeserie(x, data, date))
    }

    timeserie_i <- .all_timeserie(formula, data, date)
    name_i <- names(timeserie_i)
    .sc(Map(.attribute_name, timeserie_i, name_i), "list.timeserieb")
}

# lag to vars
# not export

.get_lagb <- function(x, k) {
    if (k == 0) {
        return(x)
    } else if (k < 0) {
        na_i <- rep(NA, abs(k))
        n_i <- length(x)
        k_i <- abs(k)
        x_i <- x[1:(n_i-k_i)]
        c(na_i, x_i)
    } else if (k > 0) {
        na_i <- rep(NA, abs(k))
        n_i <- length(x)
        k_i <- abs(k)+1
        x_i <- x[k_i:n_i]
        c(x_i, na_i)
    }
}

# get acf
# not export

.get_acfb <- function(object, data, lag.max, group, method, ci, ...) {
    if (group == "acf") {
        acf_i <- .acf(object, data, ci, ...)
        name_i <- names(acf_i)
        .sc(Map(.attribute_name, acf_i, name_i), "list.acfb")
    } else if (group == "pacf") {
        pacf_i <- .pacf(object, data, ci, ...)
        name_i <- names(pacf_i)
        .sc(Map(.attribute_name, pacf_i, name_i), "list.pacfb")
    } else if (group == "ccf") {
        ccf_i <- .ccf(object, data, lag.max, ci, method, ...)
        name_i <- names(ccf_i)
        .sc(Map(.attribute_name, ccf_i, name_i)[-1], "list.ccfb")
    } else {
        stop("Unknown group.")
    }
}

# select transform to the estimate
# not export

.method_xy <- function(x, y, method) {
    if (method == "default") {
        list(x = x, y = y)
    } else if (method == "prewhiten") {
        fit_x <- ar(x)
        x_i <- fit_x[["resid"]]
        y_i <- filter(y, filter = c(1, -fit_x[["ar"]]),
            method = "convolution", sides = 1)
        select_i <- fit_x[["order"]]+1
        n_i <- length(y)
        x_i <- x_i[select_i:n_i]
        y_i <- y_i[select_i:n_i]
        list(x = as.vector(x_i), y = as.vector(y_i))
    }
}

# get residual for covariable
# not export

.get_univariateb <- function(formula, data, d, ci, f, ...) {
    .diff <- function(data, d) {
        l_i <- lapply(data, function(x).select_diff(x, d))
        as.data.frame.list(l_i, check.names = FALSE)
    }

    data_i <- .select_variable(formula, data, NULL)
    name_i <- .get_variable(formula, data)
    f_i <- as.formula(paste0(name_i[["y"]], "~", "1"))
    diff_i <- data.frame(
        time = 1:nrow(data),
        d = if (is.null(d)) 0 else d, 
        .diff(data_i[["data"]], d),
        check.names = FALSE)
    acf_i <- .acf(f_i, na.omit(diff_i), ci, ...)
    pacf_i <- .pacf(f_i, na.omit(diff_i), ci, ...)
    obj_i <- list(
        d = diff_i,
        acf = acf_i,
        pacf = pacf_i,
        call = f,
        vars.names = name_i,
        data = list(full = data))
    .sc(obj_i, "arimab.univariateb")
}

# get residual for covariable
# not export

.get_covariableb <- function(formula, data, ci, f, ...) {
    fit_lm <- lm(formula, data)
    r_i <- residuals(fit_lm)
    df_i <- .select_data(formula, data)
    data_i <- data.frame(df_i, residual = r_i, check.names = FALSE)
    acf_i <- .acf(formula, data_i, ci, ...)[["residual"]]
    pacf_i <- .pacf(formula, data_i, ci, ...)[["residual"]]
    obj_i <- list(
        object = fit_lm,
        residual = r_i,
        acf = acf_i,
        pacf = pacf_i,
        call = f,
        vars.names = .get_variable(formula, data),
        data = list(data = df_i, full = data))
    .sc(obj_i, "arimab.covariableb")
}

# fit arima
# not export

.get_arimab <- function(formula, data, order, seasonal, include_mean, f, ...) {
    .call_to_f <- function(object) {
        f_i <- suppressWarnings(deparse(object[["call"]]))
        f_i <- toString(f_i)
        f_i <- gsub("\\, ,", "\\,", f_i)
        paste0(f_i, "[['data']][['y']]")
    }

    .seasonal_to_arima <- function(object) {
        if (is.null(object)) {
            return(list(order = c(0L, 0L, 0L), period = NA))
        } else {
            return(object)
        }
    }

    list_i <- .select_variable(formula, data, f)
    y_i <- list_i[["y"]]
    xreg_i <- if (anyNA(list_i)) NULL else list_i[["x"]]
    order_i <- .select_order(y_i, xreg_i, order, seasonal)
    fit_arima <- arima(x = y_i, order = order_i[["order"]], 
        seasonal = .seasonal_to_arima(seasonal), 
        xreg = xreg_i, include.mean = include_mean, ...)
    fit_arima2 <- fit_arima[.vars_arima]
    fit_arima2[["residuals"]] <- as.vector(fit_arima2[["residuals"]])
    fit_arima2[["fitted"]] <- y_i - fit_arima2[["residuals"]]
    fit_arima2[["call"]] <- list_i[["call"]]
    fit_arima2[["terms"]] <- terms(formula, data = data)
    fit_arima2[["order"]] <- .select_order_name(order_i)
    fit_arima2[["nobs"]] <- length(y_i)
    fit_arima2[["vars.names"]] <- .get_variable(formula, data)
    fit_arima2[["series"]] <- suppressWarnings(.call_to_f(fit_arima2))
    fit_arima2[["data"]] <- list_i
    class(fit_arima2) <- c("arimab.model", "Arima")
    return(fit_arima2)
}

# summary from arima
# not export

.get_summary_arima <- function(object, ...) {
    call_i <- object[["call"]]
    order_i <- object[["order"]]
    sigma_i <- object[["sigma2"]]
    loglik_i <- logLik(object)
    data_i <- object[["data"]]
    aic_i <- AIC(object)
    bic_i <- BIC(object)
    coef_i <- coefficients(object)
    n_coef <- length(coef_i)
    n_i <- nobs(object)
    gl_i <- n_i-n_coef
    se_i <- sqrt(diag(vcov(object)))
    t_value <- coef_i/se_i
    p_value <- 2*pt(abs(t_value),
        df = gl_i, 
        lower.tail = FALSE)
    r_i <- residuals(object)
    term_i <- terms(object)
    df_coef <- data.frame(
        "Estimate" = coef_i,
        "Std.Error" = se_i,
        "t.value" = t_value,
        "Pr(>|t|)" = p_value,
        check.names = FALSE)
    .sc(list(
        call = call_i,
        order = order_i,
        sigma2 = sigma_i,
        loglik = loglik_i,
        coefficient = df_coef,
        residual = r_i,
        aic = aic_i,
        bic = bic_i,
        terms = term_i,
        data = data_i),
        "summary.arimab.model")
}

# diagnostic arima
# not export

.get_diagnosticb <- function(object, name_obj, ci, f) {
    r_i <- residuals(object)
    acf_r <- acf(r_i, plot = FALSE)
    acf_i <- as.vector(acf_r[["acf"]])
    lag_i <- as.vector(acf_r[["lag"]])
    rs_i <- rstandard(object)
    ci_i <- .ic_acf(acf_r, ci)
    p_i <- .pvalue(r_i, lag_i)
    order_i <- object[["order"]]
    data_i <- object[["data"]]
    obj_i <- list(
        object = name_obj,
        residual = r_i, 
        residual_standard = rs_i,
        residual_acf = acf_i,
        ci = ci_i,
        lag = lag_i,
        pvalue = p_i,
        call = f,
        order = order_i,
        data = data_i)
    .sc(obj_i, "arimab.diagnosticb")
}

# extract data from object
# not export

.get_extractb <- function(object, newdata) {
    .f <- function(object) {
        f_i <- object[["vars.names"]][["formula"]]
        f_i <- as.formula(paste(f_i, collapse = ""))
        v_i <- all.vars(f_i)
        list(f = f_i, v = v_i)        
    }

    .prep <- function(object, newdata) {
        f_i <- .f(object)
        df_full <- as.data.frame(object[["data"]][["full"]][,f_i[["v"]]], make.names = FALSE)
        df_new <- as.data.frame(newdata[,f_i[["v"]]], make.names = FALSE)
        row.names(df_full) <- paste0("train", 1:nrow(df_full))
        row.names(df_new) <- paste0("predict", 1:nrow(df_new))
        list(f = f_i, data = rbind(df_full, df_new))
    }

    .pred <- function(object, newdata) {
        p_i <- .prep(object, newdata)
        f_i <- p_i[["f"]][["f"]]
        df_pred <- p_i[["data"]]
        l_i <- grepl(row.names(df_pred), pattern = "predict")
        df_i <- model.frame(f_i, df_pred, na.action = na.pass)
        df_exp <- df_i[which(l_i),]
        row.names(df_exp) <- 1:nrow(df_exp)
        return(df_exp)
    }

    .pred(object, newdata)
}

# predict in stats to the arima
# not export

.predict <- function (object, n.ahead = 1L, newxreg = NULL, se.fit = TRUE, ...) {
    myNCOL <- function(x) if (is.null(x)) 0 else ncol(x)
    rsd <- as.ts(residuals(object))
    xr <- object[["data"]][["x"]]
    xreg <- if (!is.null(xr) & !anyNA(xr)) eval.parent(xr) else NULL
    ncxreg <- myNCOL(xreg)
    if (myNCOL(newxreg) != ncxreg) 
        stop("'data' and 'newdata' have different numbers of columns")
    xtsp <- tsp(rsd)
    n <- length(rsd)
    arma <- object[["arma"]]
    coefs <- coef(object)
    narma <- sum(arma[1L:4L])
    if (length(coefs) > narma) {
        if (names(coefs)[narma + 1L] == "intercept") {
            newxreg <- cbind(intercept = rep(1, n.ahead), newxreg)
            ncxreg <- ncxreg + 1L
        }
        xm <- if (narma == 0) 
            drop(as.matrix(newxreg) %*% coefs)
        else drop(as.matrix(newxreg) %*% coefs[-(1L:narma)])
    } else {
        xm <- 0
    }

    if (arma[2L] > 0L) {
        ma <- coefs[arma[1L] + 1L:arma[2L]]
        if (any(Mod(polyroot(c(1, ma))) < 1)) 
            warning("MA part of model is not invertible")
    }

    if (arma[4L] > 0L) {
        ma <- coefs[sum(arma[1L:3L]) + 1L:arma[4L]]
        if (any(Mod(polyroot(c(1, ma))) < 1)) 
            warning("seasonal MA part of model is not invertible")
    }

    z <- KalmanForecast(n.ahead, object[["model"]])
    pred <- ts(z[[1L]] + xm, start = xtsp[2L] + deltat(rsd), 
        frequency = xtsp[3L])
    if (se.fit) {
        se <- ts(sqrt(z[[2L]] * object$sigma2), start = xtsp[2L] + 
            deltat(rsd), frequency = xtsp[3L])
        list(pred = pred, se = se)
    } else {
        pred
    }
}

# predict arima
# not export

.get_accuracy <- function(object, xf, xp, y, yp, h) {
    .h <- function(object, h) {
        if (is.null(h)) {
            return(NA)
        } else if (!is.null(h)) {
            return(h)
        }
    }

    .me <- function(x, y, h) {
        sum(y - x)/h
    }

    .mae <- function(x, y, h) {
        sum(abs(y - x))/h
    }

    .mse <- function(x, y, h) {
        sum(y - x)^2/h
    }

    .rmse <- function(x, y, h) {
        sqrt(sum((y - x)^2)/h)
    }

    rbind(
        data.frame(
            n = nobs(object),
            me = .me(xf, y, nobs(object)),
            mse = .mse(xf, y, nobs(object)),
            rmse = .rmse(xf, y, nobs(object)),
            mae = .mae(xf, y, nobs(object))
        ),
        data.frame(
            n = .h(object, h),
            me = .me(xp, yp, .h(object, h)),
            mse = .mse(xp, yp, .h(object, h)),
            rmse = .rmse(xp, yp, .h(object, h)),
            mae = .mae(xp, yp, .h(object, h))
        )
    )
}

# predict arima
# not export

.get_predict_arima <- function(object, newdata,
    h, date, by, ...) {
    .head <- function(x, h) {
        if (is.null(h)) {
            return(x)
        } else if (!is.null(h)) {
            head(x, h)
        }        
    }

    name_x <- object[["vars.names"]][["x"]]
    f_x <- paste0(name_x, collapse = "+")
    f_x <- paste0("~", f_x)
    y_i <- object[["data"]][["y"]]
    yp <- .head(newdata[[object[["vars.names"]][["y"]]]], h)
    yp <- if (is.null(yp)) NA else yp
    fit_i <- fitted(object)
    date_i <- .select_dateAdd(object, h, date, by)
    if (!is.null(newdata)) {
        newdata_i <- .head(newdata[,name_x,drop=FALSE], h)
        .sc(.select_predict(object, newdata_i, h,
            date_i, y_i, yp, fit_i, ...), "arimab.predict")
    } else if (is.null(newdata)) {
        .sc(.select_predict(object, newdata, h,
            date_i, y_i, yp, fit_i, ...), "arimab.predict")
    }
}

# time serie graphic
# not export

.get_ggtimeserie <- function(object, ncol, nrow, color_line,
    color_measure, size_measure, size_text_x, size_text_y, 
    size_title_x, size_title_y, info) {
    do.call(gridExtra::grid.arrange, 
        c(ncol = ncol, nrow = nrow, lapply(object, .ggtimeserie,
            color_line, color_measure, size_measure, size_text_x, size_text_y, 
            size_title_x, size_title_y, info)))
}

# acf graphic
# not export

.get_ggacfb <- function(object, ncol, nrow,
    color_line, color_measure, size_measure,
    size_text_x, size_text_y, size_title_x,
    size_title_y, width_col, info) {
    do.call(gridExtra::grid.arrange, 
        c(ncol = ncol, nrow = nrow, lapply(object, .ggacfb,
            color_line, color_measure, size_measure, size_text_x,
            size_text_y, size_title_x, size_title_y, width_col, info)))
}

# univariate graphic
# not export

.get_ggunivariateb <- function(object, ncol, nrow, 
    color_series, color_acf, color_pacf,
    color_measure, size_measure, size_text_x,
    size_text_y, size_title_x, size_title_y,
    width_col, model_name, model_number, info) {
    
    name_y <- object[["vars.names"]][["y"]]
    if (is.null(model_name) & is.null(model_number)) {
        gridExtra::grid.arrange(
            .ggtimeseriediff(object, color_series,
                color_measure, size_measure, size_text_x,
                size_text_y, size_title_x, size_title_y, info),
            .ggacfb(object[["acf"]][[name_y]], color_acf, color_measure, 
                size_measure, size_text_x, size_text_y, 
                size_title_x, size_title_y, width_col, info),
            .ggacfb(object[["pacf"]][[name_y]], color_pacf, color_measure,
                size_measure, size_text_x, size_text_y, 
                size_title_x, size_title_y, width_col, info),
            nrow = nrow, ncol = ncol
        )
    } else {
        gridExtra::grid.arrange(
            .ggtimeseriediff(object, color_series,
                color_measure, size_measure, size_text_x,
                size_text_y, size_title_x, size_title_y, info),
            .ggacfb(object[["acf"]][[name_y]], color_acf, color_measure, 
                size_measure, size_text_x, size_text_y, 
                size_title_x, size_title_y, width_col, info),
            .ggacfb(object[["pacf"]][[name_y]], color_pacf, color_measure,
                size_measure, size_text_x, size_text_y, 
                size_title_x, size_title_y, width_col, info),
            nrow = nrow, ncol = ncol,
            top = grid::textGrob(.format_title(model_name, model_number),
                gp = grid::gpar(col = "gray30", fontsize = 8, font = 1))
        )
    }
}

# lm residual graphic
# not export

.get_ggcovariableb <- function(object, ncol, nrow, 
    color_acf, color_pacf, color_measure, size_measure, 
    size_text_x, size_text_y, size_title_x, size_title_y, 
    width_col, model_name, model_number, info) {
    
    if (is.null(model_name) & is.null(model_number)) {
        gridExtra::grid.arrange(
            .ggacfb(object[["acf"]], color_acf, color_measure, 
                size_measure, size_text_x, size_text_y, 
                size_title_x, size_title_y, width_col, info),
            .ggacfb(object[["pacf"]], color_pacf, color_measure,
                size_measure, size_text_x, size_text_y, 
                size_title_x, size_title_y, width_col, info),
            nrow = nrow, ncol = ncol
        )
    } else {
        gridExtra::grid.arrange(
            .ggacfb(object[["acf"]], color_acf, color_measure, 
                size_measure, size_text_x, size_text_y, 
                size_title_x, size_title_y, width_col, info),
            .ggacfb(object[["pacf"]], color_pacf, color_measure,
                size_measure, size_text_x, size_text_y, 
                size_title_x, size_title_y, width_col, info),
            nrow = nrow, ncol = ncol,
            top = grid::textGrob(.format_title(model_name, model_number),
                gp = grid::gpar(col = "gray30", fontsize = 8, font = 1))
        )
    }
}

# diagnostic graphic
# not export

.get_ggdiagnosticb <- function(object, lag, ncol, nrow,
    color_line, color_col, color_measure, width_col,
    size_measure, size_text_x, size_text_y, size_title_x,
    size_title_y, model_name, model_number, info) {

    if (is.null(model_name) & is.null(model_number)) {
        gridExtra::grid.arrange(
            .timeseriediag(object, color_line, color_measure,
                size_measure, size_text_x, size_text_y, 
                size_title_x, size_title_y, info),
            .acfdiag(object, color_col, color_measure, 
                width_col, size_measure, size_text_x, 
                size_text_y, size_title_x, size_title_y, info),
            .pvaluediag(object, lag, color_measure, 
                size_measure, size_text_x, size_text_y, 
                size_title_x, size_title_y, info),
            nrow = nrow, ncol = ncol)
    } else {
        gridExtra::grid.arrange(
            .timeseriediag(object, color_line, color_measure,
                size_measure, size_text_x, size_text_y, 
                size_title_x, size_title_y, info),
            .acfdiag(object, color_col, color_measure, 
                width_col, size_measure, size_text_x,
                size_text_y, size_title_x, size_title_y, info),
            .pvaluediag(object, lag, color_measure, 
                size_measure, size_text_x, size_text_y, 
                size_title_x, size_title_y, info),
            nrow = nrow, ncol = ncol,
            top = grid::textGrob(.format_title(model_name, model_number),
                gp = grid::gpar(col = "gray30", fontsize = 8, font = 1)))
    }
}

# predict graphic
# not export

.get_ggpredict <- function(object, metric, color_y, color_fitted,
    color_test, color_predict, color_measure, color_ic, size_measure,
    model_name, model_number, info) {

    if (is.null(model_name) & is.null(model_number)) {
        .ggpredict(object, metric, color_y, color_fitted, color_test, 
            color_predict, color_measure, color_ic, size_measure, info)
    } else {
        .sc(gridExtra::grid.arrange(
            .ggpredict(object, metric, color_y, color_fitted, color_test, 
                color_predict, color_measure, color_ic, size_measure, info),
            top = grid::textGrob(.format_title(model_name, model_number),
                gp = grid::gpar(col = "gray30", fontsize = 8, font = 1)),
            nrow = 1, ncol = 1), "ggpredict")
    }
}
