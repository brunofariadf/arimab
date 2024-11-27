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

# select date from data
# not export

.select_date <- function(x, data, date) {
    id_i <- as.numeric(row.names(data))
    name_i <- colnames(data)
    x_i <- x[id_i]
    df_i <- data.frame(
        date = x_i, data,
        check.names = FALSE)
    colnames(df_i) <- c(date, name_i)
    return(df_i)
}

# select numeric column from data
# not export

.select_numeric <- function(data) {
    if (is.data.frame(data)) {
        class_i <- lapply(lapply(data, class), toString)
        class_i <- as.vector(unlist(class_i))
        position_i <- which(class_i == "numeric")
        return(data[,position_i, drop = FALSE])
    }
}

# variable separate
# not export

.get_variable <- function(formula, data) {
    f_i <- as.formula(formula)
    y_i <- deparse(f_i[[2]])
    x_i <- labels(terms(f_i, data = data))
    all_i <- all.vars(as.formula(f_i))
    formula_i <- paste(capture.output(as.formula(f_i)), collapse = "")
    if (length(all_i) == 1) {
        list(y = y_i, x = NA, formula = formula_i)
    } else {
        list(y = y_i, x = x_i, formula = formula_i)
    }
}

# select variable
# not export

.select_variable <- function(formula, data, f) {
    v_i <- .get_variable(formula, data)
    df_i <- model.frame(formula, data)
    row.names(df_i) <- 1:nrow(df_i)
    if (anyNA(v_i)) {
        list(
            call = f, y = df_i[[v_i[["y"]]]], 
            x = NA, data = df_i, full = data)
    } else {
        x_i <- as.matrix(df_i[,v_i[["x"]]])
        colnames(x_i) <- v_i[["x"]]
        y_i <- df_i[[v_i[["y"]]]]
        list(call = f, y = y_i, 
            x = x_i, data = df_i, full = data)
    }
}

# select data from object
# not export

.select_data <- function(object, data) {
    if (.is_formula(object)) {
        v_i <- .get_variable(object, data)
        df_i <- model.frame(object, data)
        row.names(df_i) <- 1:nrow(df_i)
        list(y = v_i[["y"]], data = df_i)
    } else if (.is_arimab(object)) {
        if (.is_univariate(object)) {
            y_i <- object[["data"]][["y"]]
            name_y <- object[["vars.names"]][["y"]]
            df_i <- data.frame(y_i)
            colnames(df_i) <- c(name_y)
            return(df_i)
        } else if (.is_multiple(object)) {
            y_i <- object[["data"]][["y"]]
            x_i <- object[["data"]][["x"]]
            name_y <- object[["vars.names"]][["y"]]
            name_x <- object[["vars.names"]][["x"]]
            df_i <- data.frame(y_i, x_i)
            colnames(df_i) <- c(name_y, name_x)
            return(df_i)
        }
    }
}

# select diff
# not export

.select_diff <- function(x, d) {
    if (is.null(d)) {
        return(x)
    } else if (d == 0) {
        return(x)
    } else if (d > 0) {
        na_i <- rep(NA, abs(d))
        x_i <- diff(x, differences = d)
        c(na_i, x_i)
    }
}

# select 'y' name from object to the acf
# not export

.select_acf_y <- function(object, data) {
    if (.is_formula(object)) {
        v_i <- .get_variable(object, data)
        v_i[["y"]]
    } else if (.is_arimab(object)) {
        object[["vars.names"]][["y"]]
    }
}

# select order to the auto.arima
# not export

.select_order <- function(y, reg, order, seasonal) {
    if (is.null(order)) {
        auto_i <- suppressMessages(forecast::auto.arima(
                    y, 
                    seasonal = FALSE,
                    xreg = reg,
                    test = "adf"))
        order_i <- suppressMessages(forecast::arimaorder(auto_i))
        list(auto = auto_i, order = order_i)
    } else {
        list(auto = NA, order = order, seasonal = seasonal)
    }
}

# order to name
# not export

.select_order_name <- function(object) {
    .period <- function(object, seasonal) {
        if (is.list(object[["seasonal"]])) {
            if (is.null(seasonal)) {
                return(NULL)
            } else  if (length(object[["seasonal"]][["period"]]) >= 1) {
                p_i <- paste0("[", object[["seasonal"]][["period"]],"]")
                return(p_i)
            } else {
                return(NULL)
            }
        } else {
            return(NULL)
        }
    }

    .seasonal <- function(object) {
        if (is.null(object[["seasonal"]])) {
            return(NULL)
        } else if (is.list(object[["seasonal"]])) {
            return(object[["seasonal"]])
        } else {
            list_i <- list(order = object[["seasonal"]])
            return(list_i)
        }
    }

    sea_i <- .seasonal(object)
    if (is.null(sea_i)) {
        name_i <- paste0("(", toString(object[["order"]]), ")")
        order_i <- list(name = name_i, order = object[["order"]])
        return(order_i)
    } else {
        name_i <- paste0("(", toString(object[["order"]]), ")x(", 
            toString(sea_i[["order"]]), ")", .period(object, sea_i))
        order_i <- list(name = name_i, order = object[["order"]], seasonal = sea_i)
        return(order_i)
    }
}

# select the model name
# not export

.select_model_name <- function(object) {
    if (.is_arimab(object)) {
        "arimab"
    } else if (.is_formula(object)) {
        "explore"
    }
}

# select subtitle
# not export

.select_subtitle <- function(x, reference) {
    if (isTRUE(reference)) {
        return(x)
    } else if (isFALSE(reference))
        return(NULL)
}

# select labs
# not export

.select_labs <- function(object) {
    group_i <- object[["group"]]
    if (is.element(group_i, c("acf", "pacf"))) {
        object[["vars.names"]]
    } else if (is.element(group_i, "ccf")) {
        object[["vars.names"]]
        # name_y <- object[["y"]]
        # name_x <- object[["vars.names"]]
        # paste0(name_y, " ~ ", name_x)
    }
}

# message select to the acfb
# not export

.select_message_acfb <- function(object) {
    m_i <- object[["fit"]]
    if (is.element(m_i, "arimab")) {
        .select_message_arimab(object)
    } else if (m_i == "explore") {
        "exploratory data analysis"
    }
}

# message select to the arimab
# not export

.select_message_arimab <- function(object) {
    check_i <- eval(object[["call"]][["order"]])
    order_name <- object[["order"]][["name"]]
    xreg_i <- object[["data"]][["x"]]
    if (is.null(check_i) && !anyNA(xreg_i)) {
        paste0("regression with auto.arima", order_name, " errors")
    } else if (!is.null(check_i) && !anyNA(xreg_i)) {
        paste0("regression with arima", order_name," errors")
    } else if (is.null(check_i) && anyNA(xreg_i)) {
        paste0("estimate with auto.arima", order_name)
    } else if (!is.null(check_i) && anyNA(xreg_i)) {
        paste0("estimate with arima", order_name)
    } else {
        "Unknown estimate"
    }
}

# select vars.names
# not export

.attribute_name <- function(object, x) {
    object[["vars.names"]] <- x
    return(object)
}

# format name to the plot
# not export

.format_title <- function(model_name, model_number) {
    if (is.null(model_name)) {
        if (is.null(model_number)) {
            text_i <- paste0("model", "$_{", 1, "}$")
        } else {
            text_i <- paste0("model", "$_{", model_number, "}$")
        }
    } else if (!is.null(model_name)) {
        if (is.null(model_number)) {
            text_i <- paste0(model_name, "$_{", 1, "}$")
        } else {
            text_i <- paste0(model_name, "$_{", model_number, "}$")
        }
    }

    title_i <- latex2exp::TeX(text_i)
    return(title_i)
}

# add date to the predict
# not export

.select_dateAdd <- function(object, h, date, by) {
    .get_date <- function(object, date) {
        if (!is.null(date)) {
            data_i <- deparse(object[["call"]][["data"]])
            data_i <- eval(parse(text = data_i))
            date_i <- data_i[[date]]
            return(date_i)
        } else {
            n_i <- nobs(object)
            time_i <- 1:n_i
            return(time_i)
        }
    }

    .check_by <- function(x, by) {
        if (.is_date(x) || lubridate::is.POSIXct(x) ||
            lubridate::is.POSIXlt(x) || lubridate::is.POSIXt(x)) {
                return(by)
        } else {
            return(1L)
        }
    }

    .check_seq <- function(x, y, h, by) {
        if (!is.null(h)) {
            seq_i <- seq(x, 
                by = .check_by(y, by),
                length = (h+1))[-1]
        } else {
            NA
        }
    }

    .message_date <- function(date) {
        if (is.null(date)) {
            return('time')
        } else {
            return(sQuote(date))
        }
    }

    .select_date <- function(x, h, by) {
        if (.is_date(x) || lubridate::is.POSIXct(x) ||
            lubridate::is.POSIXlt(x) || lubridate::is.POSIXt(x)) {
            date_i <- as.Date(x)
            last_i <- tail(date_i, 1)
            seq_i <- .check_seq(last_i, date_i, h, by)
            list(date = date_i, seq = seq_i)
        } else {
            date_i <- x
            last_i <- tail(date_i, 1)
            seq_i <- .check_seq(last_i, date_i, h, by)
            list(date = date_i, seq = seq_i)
        }
    }

    date_i <- .get_date(object, date)
    .select_date(date_i, h, by)
}

# predict from arimab
# not export

.select_predict <- function(object, newdata,
    h, date_i, y_i, yp, fit_i, ...) {

    if (.is_univariate(object)) {
        if (is.null(h)) {
            list(
                call = object[["call"]],
                order = object[["order"]],
                h = h,
                accuracy = .get_accuracy(object, fit_i, NA, y_i, yp, h),
                # accuracy = .get_accuracy(object, newdata, h),
                date = date_i,
                y = y_i,
                ytest = yp,
                ypredict = yp,
                fitted = fit_i,
                predict = NA,
                se = NA,
                data = object[["data"]])
        } else if (!is.null(h)) {
            pred_ <- .predict(object, n.ahead = h, ...)
            pred_i <- as.vector(pred_[["pred"]])
            se_i <- as.vector(pred_[["se"]])
            list(
                call = object[["call"]],
                order = object[["order"]],
                h = h,
                accuracy = .get_accuracy(object, fit_i, pred_i, y_i, yp, h),
                # accuracy = .get_accuracy(object, newdata, h),
                date = date_i,
                y = y_i,
                ytest = yp,
                fitted = fit_i,
                predict = pred_i,
                se = se_i,
                data = object[["data"]])
        }
    } else if (.is_multiple(object)) {
        if (is.null(newdata)) {
            if (is.null(h)) {
                list(
                    call = object[["call"]],
                    order = object[["order"]],
                    h = h,
                    accuracy = .get_accuracy(object, fit_i, NA, y_i, yp, h),
                    # accuracy = .get_accuracy(object, newdata, h),
                    date = date_i,
                    y = y_i,
                    ytest = yp,
                    fitted = fit_i,
                    predict = NA,
                    se = NA,
                    data = object[["data"]])
            } else if (!is.null(h)) {
                stop("'newdata' cannot be null argument.")
            }
        } else if (!is.null(newdata)) {
            if (is.null(h)) {
                list(
                    call = object[["call"]],
                    order = object[["order"]],
                    h = h,
                    accuracy = .get_accuracy(object, fit_i, NA, y_i, yp, h),
                    # accuracy = .get_accuracy(object, newdata, h),
                    date = date_i,
                    y = y_i,
                    ytest = yp,
                    fitted = fit_i,
                    predict = NA,
                    se = NA,
                    data = object[["data"]])
            } else if (!is.null(h)) {
                pred_ <- .predict(object, n.ahead = h, newxreg = newdata, ...)
                pred_i <- as.vector(pred_[["pred"]])
                se_i <- as.vector(pred_[["se"]])
                list(
                    call = object[["call"]],
                    order = object[["order"]],
                    h = h,
                    accuracy = .get_accuracy(object, fit_i, pred_i, y_i, yp, h),
                    # accuracy = .get_accuracy(object, newdata, h),
                    date = date_i,
                    y = y_i,
                    ytest = yp,
                    fitted = fit_i,
                    predict = pred_i,
                    se = se_i,
                    data = object[["data"]])
            }
        }
    }
}

# select vars from dfpredict
# not export

.select_predict_vars <- function(object, data) {
    ytest_i <- object[["ytest"]]
    ytest_i <- if (anyNA(ytest_i)) NULL else "test"
    predict_i <- object[["predict"]]
    predict_i <- if (anyNA(predict_i)) NULL else "predict"
    reference_i <- c("y", "fitted", "ic", ytest_i, predict_i)
    data[data[["variable"]] %in% reference_i,]
}

# select predict to the plot
# not export

.select_dfpredict <- function(object) {
    date_i <- object[["date"]]
    y_i <- object[["y"]]
    y_test <- object[["ytest"]]
    fitted_i <- object[["fitted"]]
    predict_i <- object[["predict"]]
    se_i <- object[["se"]]
    low_i <- predict_i - 1.96*se_i
    upper_i <- predict_i + 1.96*se_i
    rbind(
        data.frame(date = date_i[["date"]], variable = "y", 
            value = y_i, low = NA, upper = NA),
        data.frame(date = date_i[["date"]], variable = "fitted", 
            value = fitted_i, low = NA, upper = NA),
        data.frame(date = date_i[["seq"]], variable = "test", 
            value = y_test, low = NA, upper = NA),
        data.frame(date = date_i[["seq"]], variable = "predict", 
            value = predict_i, low = NA, upper = NA),
        data.frame(date = date_i[["seq"]], variable = "ic", 
            value = NA, low = low_i, upper = upper_i)
    )
}
