#' Calculate and Print the Percentage of Missing Data Within a Data Structure
#'
#' Calculates the percentage of NA values within each column or row of a data
#' structure.
#'
#' @param x A matrix-like R object with two dimensions to get the missing data
#' percentage for.
#' @param by.row A logical value indicating whether to calculate the missing
#' data percentage by row.
#' @param row.range A numeric vector containing the row numbers to be included
#' in the missing percentage calculation.
#' @param col.range A numeric vector containing the column numbers to be
#' included in the missing percentage calculation.
#'
#' @return Returns a data frame with the columns sharing the same column or row
#' names as the passed data and a single row containing the percentage of
#' elements within a row or column with NA values.
#'
#' @examples
#' # Default by column
#' na_pct <- naPercentage(data)
#'
#' # By row, excluding first column
#' na_pct_byrow <- naPercentage(data_byrow, by.row = TRUE, col.range = 2:ncol(x))
#'
naPercentage <- function(x, by.row = FALSE, row.range = 1:nrow(x),
                                  col.range = 1:ncol(x)) {
  if (by.row) {
    np <- data.frame(matrix(nrow = 1, ncol = length(row.range)))
    for(i in row.range) {
      np[1,i] <- sum(is.na(x[i, ])) / ncol(x) * 100
    }

    colnames(np) <- rownames(x)
  }
  else {
    np <- data.frame(matrix(nrow = 1, ncol = length(col.range)))
    for(i in col.range) {
      np[1,i] <- sum(is.na(x[, i])) / nrow(x) * 100
    }

    colnames(np) <- colnames(x)
  }

  return(np)
}
