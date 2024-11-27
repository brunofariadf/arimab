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

# plot to the time serie
# not export

ggtimeserie <- function(object, ncol = NULL, nrow = NULL, 
    color_line = "deepskyblue2", color_measure = "#a8a7a7",
    size_measure = 6, size_text_x = NULL, size_text_y = NULL, 
    size_title_x = NULL, size_title_y = NULL, info = TRUE) {
    .check_ggtimeserie(object, ncol, nrow, color_line,
        color_measure, size_measure, size_text_x, size_text_y, 
        size_title_x, size_title_y, info)
}

# plot to the acf
# not export

ggacfb <- function(object, ncol = NULL, nrow = NULL, 
    color_col = "deepskyblue2", color_measure = "#a8a7a7",
    size_measure = 6, size_text_x = NULL, size_text_y = NULL, 
        size_title_x = NULL, size_title_y = NULL, width_col = 0.7, info = TRUE) {
    .check_ggacfb(object, ncol, nrow, 
        color_col, color_measure, size_measure, 
        size_text_x, size_text_y, 
        size_title_x, size_title_y, width_col, info)
}

# plot to univariate
# not export

ggunivariateb <- function(object, ncol = NULL, nrow = 1, 
    color_series = "deepskyblue1", color_acf = "deepskyblue2", 
    color_pacf = "deepskyblue3", color_measure = "#a8a7a7",
    size_measure = 6, size_text_x = NULL, size_text_y = NULL, 
    size_title_x = NULL, size_title_y = NULL, width_col = 0.7,
    model_name = NULL, model_number = NULL) {
    .check_ggunivariateb(object, ncol, nrow, 
        color_series, color_acf, color_pacf,
        color_measure, size_measure, size_text_x,
        size_text_y, size_title_x, size_title_y,
        width_col, model_name, model_number, info = FALSE)
}

# .ggtimeseriediff(univariateb(mpg ~ disp, mtcars, d = 1), 
#     color_line = "deepskyblue1", color_measure = "#a8a7a7",
#     size_measure = 6, size_text_x = NULL, size_text_y = NULL, 
#     size_title_x = NULL, size_title_y = NULL, info = TRUE)

# plot to the lm residual
# not export

ggcovariableb <- function(object, ncol = NULL, nrow = 1, 
    color_acf = "deepskyblue2", color_pacf = "deepskyblue3", color_measure = "#a8a7a7",
    size_measure = 6, size_text_x = NULL, size_text_y = NULL, 
        size_title_x = NULL, size_title_y = NULL, width_col = 0.7,
    model_name = NULL, model_number = NULL, info = FALSE) {
    .check_ggcovariableb(object, ncol, nrow, 
        color_acf, color_pacf, color_measure,
        size_measure, size_text_x, size_text_y, 
        size_title_x, size_title_y, width_col, 
        model_name, model_number, info)
}

# plot to the diagnostic
# not export

ggdiagnosticb <- function(object, lag = 20, ncol = NULL, nrow = 1,
    color_line = "deepskyblue2", color_col = "deepskyblue2", 
    color_measure = "#a8a7a7",  width_col = 0.7, size_measure = 6, 
    size_text_x = NULL, size_text_y = NULL, size_title_x = NULL,
    size_title_y = NULL, model_name = NULL, model_number = NULL, info = TRUE) {

    .check_ggdiagnosticb(object, lag, ncol, nrow, color_line, color_col,
        color_measure, width_col, size_measure, size_text_x,
        size_text_y, size_title_x, size_title_y, 
        model_name, model_number, info)
}

# plot to the time serie
# not export

ggpredict <- function(object, metric = "rmse", color_y = "lightgray",
    color_fitted = "#797878", color_test = "deepskyblue", 
    color_predict = "deepskyblue4", color_ic = "lightblue", 
    color_measure = "#a8a7a7", size_measure = 9,
    model_name = NULL, model_number = NULL, info = TRUE) {

    .check_ggpredict(object, metric, color_y, color_fitted, color_test,
        color_predict, color_measure, color_ic, size_measure,
        model_name, model_number, info)
}
