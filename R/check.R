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

# check time serie
# not export

.check_timeserie <- function(formula, data, date, name_data) {
    if (!.is_formula(formula)) {
        stop(.error("formula", formula))
    }

    if (!is.data.frame(data)) {
        stop(.error("df", deparse(substitute(data))))
    }

    if (!is.null(date)) {
        if (!is.character(date)) {
            stop(.error("character", date))
        }

        name_i <- colnames(data)
        if (!is.element(date, name_i)) {
            stop(.error("namecolumn", date))
        }

        class_i <- class(data[[date]])
        class_i <- all(is.element(class_i, .vars_date))
        if (!class_i) {
            stop(.error("date", date))
        }
    }

    row.names(data) <- 1:nrow(data)
    data_i <- model.frame(formula, data)
    if (!is.null(date)) {
        x_i <- data[[date]]
        df_i <- .select_date(x_i, data_i, date)
        .get_timeserie(formula, df_i, date, name_data)
    } else {
        .get_timeserie(formula, data_i, date, name_data)
    }
}

# check lag
# not export

.check_lagb <- function(x, k) {
    if (!is.numeric(x)) {
        stop(.error("numeric", "x"))
    }

    if (!is.numeric(k)) {
        stop(.error("numeric", "k"))
    }

    k_i <- abs(k)
    n_i <- length(x)
    if (k_i > n_i) {
        stop(.error("nlag", "k"))
    }

    .get_lagb(x, k)
}

# check acf
# not export

.check_acfb <- function(object, data, lag.max, group, method, ci, ...) {
    if (.is_formula(object)) {
        if (!is.null(data)) {
            data_i <- model.frame(object, data)
            .get_acfb(object, data_i, lag.max, group, method, ci, ...)
        } else {
            stop(.error("df", deparse(substitute(data))))
        }

    } else {
        .get_acfb(object, data, lag.max, group, method, ci, ...)
    }
}

# check covariable
# not export

.check_univariateb <- function(formula, data, d, ci, f, ...) {
    if (!.is_formula(formula)) {
        stop(.error("formula", "formula"))
    }

    if (.is_formula(formula)) {
        if (!is.data.frame(data)) {
            stop(.error("df", deparse(substitute(data))))
        }
    }

    if (!is.data.frame(data)) {
        stop(.error("df", deparse(substitute(data))))
    }

    if (!is.null(d)) {
        if (!is.numeric(d)) {
            stop(.error("numeric", "d"))
        }
    }

    if (!is.numeric(ci)) {
        stop(.error("numeric", "ci"))
    }

    .get_univariateb(formula, .select_numeric(data), d, ci, f, ...)
}

# check covariable
# not export

.check_covariableb <- function(formula, data, ci, f, ...) {
    if (!.is_formula(formula)) {
        stop(.error("formula", "formula"))
    }

    if (.is_formula(formula)) {
        if (!is.data.frame(data)) {
            stop(.error("df", deparse(substitute(data))))
        }
    }

    if (!is.data.frame(data)) {
        stop(.error("df", deparse(substitute(data))))
    }

    if (!is.numeric(ci)) {
        stop(.error("numeric", "ci"))        
    }

    .get_covariableb(formula, .select_numeric(data), ci, f, ...)
}

# check arima
# not export

.check_arimab <- function(formula, data, order, seasonal, include_mean, f, ...) {
    .get_arimab(formula, .select_numeric(data), order, seasonal,
        include_mean, f, ...)
}

# check arima
# not export

.check_diagnosticb <- function(object, name_obj, ci, f) {
    if (!.is_arimab(object)) {
        stop(.error("arimab", deparse(substitute(object))))
    }

    if (!is.numeric(ci)) {
        stop(.error("numeric", "ci"))
    }

    .get_diagnosticb(object, name_obj, ci, f)
}

# check arima
# not export

.check_extractb <- function(object, newdata) {
    if (!.is_arimab(object)) {
        stop(.error("arimab", object))
    }

    if (!is.data.frame(newdata)) {
        stop(.error("df", deparse(substitute(newdata))))
    }

    if (.is_univariate(object)) {
        match_i <- .match_column(object, newdata)
        n_y <- length(object[["vars.names"]][["y"]])
        n_match <- length(match_i[["match"]])
        if (n_y != n_match) {
            stop(.error("extract", match_i[["merge"]]))
        }
    }

    if (.is_multiple(object)) {
        match_i <- .match_column(object, newdata)
        n_x <- length(object[["vars.names"]][["x"]])
        n_match <- length(match_i[["match"]])
        if (n_x != n_match) {
            stop(.error("extract", match_i[["merge"]]))
        }
    }

    .get_extractb(object, newdata)
}

# check accuracy
# not export

.check_accuracy <- function(object) {
    if (!.is_arimab.predict(object)) {
        stop(.error("arimab.predict", deparse(substitute(object))))
    }

    object[["accuracy"]]
}

# check predict
# not export

.check_predict_arima <- function(object, newdata = NULL, h = NULL, ...) {
    if (!.is_arimab(object)) {
        stop(.error("arimab", deparse(substitute(object))))
    }

    if (!is.null(newdata)) {
        if (!is.data.frame(newdata)) {
            stop(.error("df", deparse(substitute(newdata))))
        }
    }

    if (!is.null(h)) {
        if (!is.numeric(h)) {
            stop(.error("numeric", "h"))
        }
    }

    .get_predict_arima(object, newdata, h, date = NULL, by = "month", ...)
}

# check time serie gg
# not export

.check_ggtimeserie <- function(object, ncol, nrow, color_line,
    color_measure, size_measure, size_text_x, size_text_y, 
    size_title_x, size_title_y, info) {

    .get_ggtimeserie(object, ncol, nrow, color_line, 
        color_measure, size_measure, size_text_x, size_text_y, 
        size_title_x, size_title_y, info)
}

# check acf gg
# not export

.check_ggacfb <- function(object, ncol, nrow,
    color_col, color_measure, size_measure, 
    size_text_x, size_text_y, size_title_x, 
    size_title_y, width_col, info) {

    .get_ggacfb(object, ncol, nrow,
        color_col, color_measure, size_measure, 
        size_text_x, size_text_y, size_title_x,
        size_title_y, width_col, info)
}

.check_ggunivariateb <- function(object, ncol, nrow, 
    color_series, color_acf, color_pacf,
    color_measure, size_measure, size_text_x,
    size_text_y, size_title_x, size_title_y,
    width_col, model_name, model_number, info) {

    .get_ggunivariateb(object, ncol, nrow, 
        color_series, color_acf, color_pacf,
        color_measure, size_measure, size_text_x,
        size_text_y, size_title_x, size_title_y,
        width_col, model_name, model_number, info)
}

# check acf to residual
# not export

.check_ggcovariableb <- function(object, ncol, nrow, 
    color_acf, color_pacf, color_measure, size_measure, 
    size_text_x, size_text_y, size_title_x, size_title_y, 
    width_col, model_name, model_number, info) {
    
    .get_ggcovariableb(object, ncol, nrow, 
        color_acf, color_pacf, color_measure, size_measure, 
        size_text_x, size_text_y, size_title_x, size_title_y,
        width_col, model_name, model_number, info)
}

# check diagnostic gg
# not export

.check_ggdiagnosticb <- function(object, lag, ncol, nrow, 
    color_line, color_col, color_measure, width_col,
    size_measure, size_text_x, size_text_y, size_title_x, 
    size_title_y, model_name, model_number, info) {
    
    .get_ggdiagnosticb(object, lag, ncol, nrow, color_line, color_col,
        color_measure, width_col, size_measure, size_text_x, size_text_y, 
        size_title_x, size_title_y, model_name, model_number, info)
}

# check predict gg
# not export

.check_ggpredict <- function(object, metric,
    color_y, color_fitted, color_test, 
    color_predict, color_measure, color_ic,
    size_measure, model_name, model_number, info) {

    .get_ggpredict(object, metric, color_y, color_fitted, color_test, 
        color_predict, color_measure, color_ic, size_measure,
        model_name, model_number, info)
}
