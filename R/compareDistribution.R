compareDistribution <- function(original, imputed, var) {
  ggplot(mapping = aes_string(var)) +
    geom_density(data = original, aes(color = "Original")) +
    geom_density(data = imputed, aes(color = "Imputed")) +
    scale_color_manual("",
                       breaks = c("Original", "Imputed"),
                       values = c("blue", "red"))

}
