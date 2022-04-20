imputationSummary <- function(original, imputed) {
  imp_summary <- data.frame(
    mean = c(original = mean(original, na.rm = TRUE), imputed = mean(imputed, na.rm = TRUE)),
    median = c(original = median(original, na.rm = TRUE), imputed = median(imputed, na.rm = TRUE)),
    sd = c(original = sd(original, na.rm = TRUE), imputed = sd(imputed, na.rm = TRUE))
  )

  return(imp_summary)
}
