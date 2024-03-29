#' Calculate the area under the curve (AUC) for each subject over the time interval for dependent variables (\code{dv}) using the trapezoidal rule.
#'
#' @param data A data frame.
#' @param time A string containing the name of the chronologically ordered time variable in \code{data}.
#' @param id A string containing the name of the ID column (defining subject level data) in \code{data}.
#' @param dv A string containing the name of the dependent variable column in \code{data}.
#'
#' @return A data frame containing one AUC value for every subject as defined by \code{id}.
#'
#' Based on the \code{AUC} function originally written by Leonid Gibiansky in package MIfuns 5.1, from Metrum Institute.
#'
#' @author Leonid Gibiansky, \email{lgibiansky@@quantpharm.com}
#' @references \url{https://code.google.com/archive/p/mifuns/}
#'
#' @examples
#' \dontrun{
#'  AUCs <- get_auc(myAUCdata)
#' }
#'
#' @import stats 
#' @importFrom magrittr `%>%`
#' @importFrom dplyr group_by_at
#' @export

get_auc <- function (data, time = "TIME", id = "ID", dv = "DV")
{
  auc_sub <- function(data) {
    data <- data[order(data[[id]], -data[[time]]), ]
    nrec <- length(data[[time]])
    data$diff <- c(data[[time]][-nrec] - data[[time]][-1], 0)
    data$meanDV <- c((data[[dv]][-1] + data[[dv]][-nrec])/2,
                     0)
    data$dAUC <- data$diff * data$meanDV
    data <- data[order(data[[id]], data[[time]]), ]
    data <- data[duplicated(data[[id]]), ]
    AUC <- aggregate.data.frame(data$dAUC, by = list(data[[id]]),
                                FUN = sum)
    names(AUC) <- c(id, "AUC")
    return(AUC)
  }
  data %>% dplyr::group_by_at(id) %>% auc_sub()
}

