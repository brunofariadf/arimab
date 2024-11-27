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

# arimab
# not export

.is_arimab <- function(object) {
    if (is.element("arimab.model", class(object))) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}

# arimab.predict
# not export

.is_arimab.predict <- function(object) {
    if (is.element("arimab.predict", class(object))) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}

# formula
# not export

.is_formula <- function(object) {
    if (is.element("formula", class(object))) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}

# type uni
# not export

.is_univariate <- function(object) {
    if (.is_arimab(object)) {
        check_x <- anyNA(object[["data"]])
        if (isTRUE(check_x)) {
            return(TRUE)
        } else {
            return(FALSE)
        }
    } else {
        stop("Method applicated to the 'arimab' model class.")
    }
}

# type mult
# not export

.is_multiple <- function(object) {
    if (.is_arimab(object)) {
        check_x <- anyNA(object[["data"]])
        if (isFALSE(check_x)) {
            return(TRUE)
        } else {
            return(FALSE)
        }
    } else {
        stop("Method applicated to the 'arimab' model class.")
    }
}

# date
# not export

.is_date <- function(x) {
    date_i <- function(x) {
        format_i = try(as.Date(x, "%Y-%m-%d"), silent = TRUE)
        check_date = as.character(format_i) == x && !is.na(format_i)
        check_date[is.na(x)] = NA
        return(check_date)
    }

    x_i <- as.vector(unlist(lapply(x, date_i)))
    if (is.element(FALSE, x_i)) {
        return(FALSE)
    } else {
        all(x_i)
    }
}
