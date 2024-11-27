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

# check error
# not export

.error_formula <- function(insert) {
    paste0(sQuote(insert), " must be formula class object.")
}

.error_df <- function(insert) {
    paste0(sQuote(insert), " must be data.frame class object.")
}

.error_namecolumn <- function(insert) {
    paste0(sQuote(insert), " column was not found in the data.frame object.")
}

.error_date <- function(insert) {
    paste0(sQuote(insert), " must be date class vector with option 'Date', 'date', 'POSIXct', 'POSIXlt', 'POSIXt'.")
}

.error_nlag <- function(insert) {
    paste0(sQuote(insert), " must be a vector with length less than or equal to 'x'.")
}

.error_character <- function(insert) {
    paste0(sQuote(insert), " must be character class vector.")
}

.error_numeric <- function(insert) {
    paste0(sQuote(insert), " must be numeric class vector.")
}

.error_true_false <- function(insert) {
    paste0(sQuote(insert), " must be logical class vector with option 'TRUE' or 'FALSE'.")
}

.error_arimab <- function(insert) {
    paste0(sQuote(insert), " must be arimab.model class object.")
}

.error_arimab.predict <- function(insert) {
    paste0(sQuote(insert), " must be arimab.predict class object.")
}

.error_extract <- function(insert) {
    paste0("The number of columns in 'newdata' differs from the variables used in 'object'. Column match: ",
        sQuote(insert))
}

.error <- function(type, insert) {
    switch(type,
        "character" = .error_character(insert),
        "numeric" = .error_numeric(insert),
        "true_false" = .error_true_false(insert),
        "formula" = .error_formula(insert),
        "df" = .error_df(insert),
        "namecolumn" = .error_namecolumn(insert),
        "date" = .error_date(insert),
        "nlag" = .error_nlag(insert),
        "arimab" = .error_arimab(insert),
        "arimab.predict" = .error_arimab.predict(insert),
        "extract" = .error_extract(insert),
        "Unknown error type."
    )
}
