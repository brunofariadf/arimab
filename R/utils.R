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

# select object to the arima
# not export

.vars_arima <- c("coef", "sigma2","var.coef","mask","loglik","aic",
    "arma","residuals","code","n.cond","model","series")

# select vars from date
# not export

.vars_date <- c("Date", "date", "POSIXct", "POSIXlt", "POSIXt")

# check number to diference in time serie
# not export

ndiference <- function(x) {
    y_i <- x
    p_i <- suppressMessages(tseries::adf.test(x,
        alternative = "stationary")[["p.value"]])
    p_value_i <- p_i
    if (p_i < 0.05 || anyNA(p_i)) {
        list_i <- list(p_value = p_value_i, n = 0)
        return(list_i)
    } else {
        i <- 2
        n_i <- numeric()
        while (p_i > 0.05) {
            y_i <- diff(y_i)
            p_i <- suppressWarnings(tseries::adf.test(y_i,
                alternative = "stationary")[["p.value"]])
            p_value_i[i] <- p_i
            n_i[i] <- i 
            i <- i + 1
        }

        list_i <- list(p_value = p_value_i, n = (max(n_i, na.rm = TRUE)-1))
        return(list_i)
    }
}

# check newdata from object
# not export

.match_column <- function(object, newdata) {
    if (.is_univariate(object)) {
        v_i <- object[["vars.names"]][["y"]]
    }

    if (.is_multiple(object)) {
        v_i <- object[["vars.names"]][["x"]]
    }
    
    f_i <- paste0("~", paste(v_i, collapse = "+"))
    all_i <- all.vars(as.formula(f_i))
    name_i <- paste(all_i, collapse = "|")
    name_newdata <- colnames(newdata)
    filter_i <- Filter(function(x)grepl(x, pattern = name_i), name_newdata)
    if (length(filter_i) >= 1) {
        pos_i <- name_newdata %in% filter_i
        pos_i <- toString(paste(name_newdata, pos_i, sep="="))
        obj_i <- list(
            match = filter_i,
            merge = pos_i)
        return(obj_i)
    } else {
        c_i <- rep(FALSE, length(name_newdata))
        pos_i <- toString(paste(name_newdata, c_i, sep="="))
        obj_i <- list(
            match = NULL,
            merge = pos_i)
        return(obj_i)
    }
}

# describe
# not export

.describe <- function(x, digits = 2) {
    c(
        n = round(length(x), digits),
        min = round(min(x, na.rm = TRUE), digits),
        median = round(median(x, na.rm = TRUE), digits),
        mean = round(mean(x, na.rm = TRUE), digits),
        max = round(max(x, na.rm = TRUE), digits)
    )
}

# set class
# not export

.sc <- function(object, item) {
    class(object) <- item
    return(object)
}
