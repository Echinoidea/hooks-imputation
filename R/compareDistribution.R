#' Compare Distributions of Original vs. Imputed Data
#'
#' Create a plot with two density layers representing the distribution of the
#' test variable for both the original and imputed data.
#'
#' @param original The original, unimputed data.
#' @param imputed The data with missing values imputed.
#' @param by The variable to plot the distributions of that both
#' \code{original} and \code{imputed} have.
#'
#' @return Returns a ggplot density plot containing a line for each data object.
#'
#' @examples
#' compareDistribution(data, data_imputed, "values")
#'
compareDistribution <- function(original, imputed, by) {
  require(ggplot2)

  ggplot(mapping = aes_string(by)) +
    geom_density(data = original, aes(color = "Original")) +
    geom_density(data = imputed, aes(color = "Imputed")) +
    scale_color_manual("",
                       breaks = c("Original", "Imputed"),
                       values = c("blue", "red"))

}
