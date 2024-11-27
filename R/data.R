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

#' Time Series dataset of the Central Bank of Brazil
#'
#' Set of Time Series data collected from the Central Bank of Brazil between 2014 and 2022 for the variables interest, inflation, exchange rate and ibc_br index.
#' 
#' @usage bacen
#'
#' @details
#' \describe{
#'   \item{date}{date with monthly periodicity}
#'   \item{rate}{Average of the Copom's Selic target for interest rate}
#'   \item{inflation}{Consumer price index in 12 months }
#'   \item{currency}{Average nominal exchange rate in sales value}
#'   \item{ibc_br}{percentage change for 12 months of the IBC-BR index of the Central Bank of Brazil}
#' }
#' @source <https://www.bcb.gov.br/en>

"bacen"