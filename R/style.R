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

## time series drawing
## not export

.ggtimeserie <- function(object, color_line, color_measure,
    size_measure, size_text_x, size_text_y, 
    size_title_x, size_title_y, info) {
    stationary_i <- tryCatch(object[[4]],
        error=function(i)"NA")
    n_i <- tryCatch(stationary_i[["n"]],
        error=function(i)"NA")
    freq_i <- object[[5]]
    df_i <- object[[6]]
    name_date <- object[[3]]
    name_y <- object[[1]]
    sub_i <- paste0("stationary = ", n_i)
    ggplot2::ggplot(df_i) +
        ggplot2::aes_string(name_date, "y") +
        ggplot2::geom_line(color = color_line) +
        ggplot2::labs(y = name_y,
            subtitle = .select_subtitle(sub_i, info)) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
            plot.subtitle = ggplot2::element_text(
                size = size_measure, color = color_measure),
            axis.text.x = ggplot2::element_text(
                size = size_text_x),
            axis.text.y = ggplot2::element_text(
                size = size_text_y),
            axis.title.x = ggplot2::element_text(
                size = size_title_x),
            axis.title.y = ggplot2::element_text(
                size = size_title_y)
        )
}

## time series diff
## not export

.ggtimeseriediff <- function(object, color_line, color_measure,
    size_measure, size_text_x, size_text_y, 
    size_title_x, size_title_y, info) {
    df_i <- object[["d"]]
    name_y <- object[["vars.names"]][["y"]]
    ggplot2::ggplot(df_i) +
        ggplot2::aes_string("time", name_y) +
        ggplot2::geom_line(color = color_line) +
        ggplot2::labs(y = name_y) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
            plot.subtitle = ggplot2::element_text(
                size = size_measure, color = color_measure),
            axis.text.x = ggplot2::element_text(
                size = size_text_x),
            axis.text.y = ggplot2::element_text(
                size = size_text_y),
            axis.title.x = ggplot2::element_text(
                size = size_title_x),
            axis.title.y = ggplot2::element_text(
                size = size_title_y)
        )
}

## acf
## not export

.ggacfb <- function(object, color_col, color_measure,
    size_measure, size_text_x, size_text_y, 
    size_title_x, size_title_y, width_col, info) {
        acf_i <- object[[1]]
        ic_i <- object[[2]][["interval"]]
        group_i <- object[[4]]
        lag_i <- object[[6]]
        df_acf <- data.frame(lag = lag_i, acf = acf_i)
        sub_i <- .select_labs(object)
        ggplot2::ggplot(df_acf,
            ggplot2::aes(x = lag, y = acf)) +
            ggplot2::geom_col(
                fill = color_col,
                width = width_col) +
            ggplot2::geom_hline(
                yintercept = 0,
                col = "black") +
            ggplot2::geom_hline(
                yintercept = ic_i,
                lty = 2, col = "deepskyblue4") +
            ggplot2::labs(y = group_i,
                subtitle = .select_subtitle(sub_i, info)) +
            ggplot2::theme_minimal() +
            ggplot2::theme(
                plot.subtitle = ggplot2::element_text(
                    size = size_measure, color = color_measure),
            axis.text.x = ggplot2::element_text(
                size = size_text_x),
            axis.text.y = ggplot2::element_text(
                size = size_text_y),
            axis.title.x = ggplot2::element_text(
                size = size_title_x),
            axis.title.y = ggplot2::element_text(
                size = size_title_y)
        )
}

## diagnostic
## not export

.timeseriediag <- function(object, color_line, color_measure,
    size_measure, size_text_x, size_text_y, 
    size_title_x, size_title_y, info) {
    r_i <- object[["residual_standard"]]
    time_i <- 1:length(r_i)
    df_i <- data.frame(residual = r_i, time = time_i)
    sub_i <- "standardized residuals"
    ggplot2::ggplot(df_i,
        ggplot2::aes(x = time, y = residual)) +
        ggplot2::geom_line(color = color_line) +
        ggplot2::labs(subtitle = .select_subtitle(sub_i, info)) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
            plot.subtitle = ggplot2::element_text(
                size = size_measure, color = color_measure),
            axis.text.x = ggplot2::element_text(
                size = size_text_x),
            axis.text.y = ggplot2::element_text(
                size = size_text_y),
            axis.title.x = ggplot2::element_text(
                size = size_title_x),
            axis.title.y = ggplot2::element_text(
                size = size_title_y)
        )
}

.acfdiag <- function(object, color_col, color_measure, 
    width_col, size_measure, size_text_x, size_text_y, 
    size_title_x, size_title_y, info) {
    lag_i <- object[["lag"]]
    acf_i <- object[["residual_acf"]]
    ic_i <- object[["ci"]][["interval"]]
    df_i <- data.frame(lag = lag_i[-1], acf = acf_i[-1])
    sub_i <- "acf of residuals"
    ggplot2::ggplot(df_i,
        ggplot2::aes(x = lag, y = acf)) +
        ggplot2::geom_col(
            fill = color_col,
            width = width_col) +
        ggplot2::geom_hline(
            yintercept = 0,
            col = "black") +
        ggplot2::geom_hline(
            yintercept = ic_i,
            lty = 2, col = "deepskyblue4") +
        ggplot2::labs(subtitle = .select_subtitle(sub_i, info)) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
            plot.subtitle = ggplot2::element_text(
                size = size_measure, color = color_measure),
            axis.text.x = ggplot2::element_text(
                size = size_text_x),
            axis.text.y = ggplot2::element_text(
                size = size_text_y),
            axis.title.x = ggplot2::element_text(
                size = size_title_x),
            axis.title.y = ggplot2::element_text(
                size = size_title_y)
        )
}

.pvaluediag <- function(object, lag, color_measure, 
    size_measure, size_text_x, size_text_y, 
    size_title_x, size_title_y, info) {
    lag_i <- object[["lag"]]
    p_i <- object[["pvalue"]]
    lag_filter <- 1:lag
    df_i <- data.frame(lag = lag_i, pvalue = p_i)
    sub_i <- "Ljung-Box test"
    ggplot2::ggplot(df_i[lag_filter,],
            ggplot2::aes(x = lag, y = pvalue)) +
        ggplot2::geom_point(colour = "deepskyblue3") +
        ggplot2::geom_hline(
            yintercept = 0.05, lty = 2,
            colour = "deepskyblue4") +
        ggplot2::labs(y = "p-value",
            subtitle = .select_subtitle(sub_i, info)) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
            plot.subtitle = ggplot2::element_text(
                size = size_measure, color = color_measure),
            axis.text.x = ggplot2::element_text(
                size = size_text_x),
            axis.text.y = ggplot2::element_text(
                size = size_text_y),
            axis.title.x = ggplot2::element_text(
                size = size_title_x),
            axis.title.y = ggplot2::element_text(
                size = size_title_y)
        )
}

## predict
## not export

.ggpredict <- function(object, metric, color_y, 
    color_fitted, color_test, color_predict, 
    color_measure, color_ic, size_measure, info) {

    df_i <- .select_dfpredict(object)
    df_i <- .select_predict_vars(object, df_i)
    acc_i <- accuracy(object)
    formula_i <- capture.output(object[["call"]][["formula"]])
    metric_i <- acc_i[[metric]]
    period_i <- object[["h"]]
    sub_i <- paste0("h = ", period_i, ", ", 
                paste0(metric, "_fitted")," = ", 
                round(metric_i[1], 4), ", ",
                paste0(metric, "_predict")," = ", 
                round(metric_i[2], 4), "\n",
                formula_i)
    ggplot2::ggplot(df_i,
        ggplot2::aes_string(x = "date", y = "value", color = "variable")) +
    ggplot2::geom_ribbon(
        ggplot2::aes_string(
            x = "date", ymin = "low", ymax = "upper"), 
            fill = color_ic, linetype = 0, alpha = 0.4) +
    ggplot2::geom_line() +
    ggplot2::scale_color_manual(
        values = c(
            "y" = color_y,
            "fitted" = color_fitted, 
            "test" = color_test, 
            "predict" = color_predict)) +
    ggplot2::labs(y = "y", x = "time",
        subtitle = .select_subtitle(sub_i, info)) +
    ggplot2::theme_minimal() +
    ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(fill=NA))) +
    ggplot2::theme(
        legend.position = "top",
        legend.title = ggplot2::element_blank(),
        plot.subtitle = ggplot2::element_text(size = size_measure, color = color_measure))
}
