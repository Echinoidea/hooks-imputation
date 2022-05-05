#' Compare Summaries of Original vs. Imputed Data
#'
#' @param original The original, unimputed data object.
#' @param imputed The data object with missing values imputed.
#'
#' @return Returns a data frame containing the mean, median, and standard
#' deviation of both data objects.
#'
#' @examples
#' compareSummary(data, data_imputed)
#' @export
compareSummary <- function(original, imputed, by) {
  imp_summary <- data.frame(
    mean = c(original = mean(original[, by], na.rm = TRUE), imputed = mean(imputed[, by], na.rm = TRUE)),
    median = c(original = median(original[, by], na.rm = TRUE), imputed = median(imputed[, by], na.rm = TRUE)),
    sd = c(original = sd(original[, by], na.rm = TRUE), imputed = sd(imputed[,by], na.rm = TRUE))
  )

  return(imp_summary)
}
