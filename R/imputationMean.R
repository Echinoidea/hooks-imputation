#' Imputation with Variable Mean
#'
#' Replace missing values in data with the mean of that element's column or row.
#'
#' @param x A matrix-like R object with two dimensions to perform imputation on.
#' @param by.row A logical value indicating whether to calculate the mean by the
#' values within an element's row or not. Default = \code{TRUE}
#' @param row.range A numeric vector containing the row numbers to be included
#' in the mean calculation and imputation.
#' @param col.range A numeric vector containing the column numbers to be
#' included in the mean calculation and imputation.
#'
#' @return Returns the passed data with missing values imputed with the mean of
#' each element's respective row or column.
#'
#' @examples
#' # Default by column
#' imputed_mean <- imputationMean(data)
#'
#' # By row, excluding the first column.
#' imputed_mean_byrow <- imputationMean(data_byrow,
#' col.range = 2:ncol(data_byrow))
imputationMean <- function(x, by.row = FALSE, row.range = 1:nrow(x), col.range = 1:ncol(x)) {
  imputed <- x

  if (by.row) {
    for(r in row.range) {
      for (c in col.range) {
        if (is.na(imputed[r, c])) {
          imputed[r, c] <- rowMeans(x[r, col.range], na.rm = TRUE)
        }
      }
    }
  }
  else {  # By column
    for(c in col.range) {
      for (r in row.range) {
        if (is.na(imputed[r, c])) {
          imputed[r, c] <- mean(x[row.range, c], na.rm = TRUE)
        }
      }
    }
  }

  return(imputed)
}
