#' Hot Deck Imputation
#'
#' Replace missing values with a randomly selected, non-NA value from that
#' element's column or row.
#'
#' @param x A matrix-like R object with two dimensions to perform imputation on.
#' @param by.row A logical value indicating whether to calculate the mean by the
#' values within an element's row or not. Default = \code{TRUE}
#' @param row.range A numeric vector containing the row numbers to be included
#' in the hot deck imputation.
#' @param col.range A numeric vector containing the column numbers to be
#' included in the hot deck imputation.
#'
#' @return Returns the passed data with missing values imputed with a random
#' value from that element's column or row.
#' @examples
imputationHotDeck <- function(x, by.row = FALSE, row.range = 1:nrow(x), col.range = 1:ncol(x)) {
  imputed <- x

  if (by.row) {
    for(r in row.range) {
      for(c in col.range) {
        if(is.na(imputed[r, c]) & is.numeric(imputed[r, c])) {
          # Set current to any column which is not na
          non_na <- imputed[r, which(is.na(imputed[r,]) != TRUE)]
          non_na <- Filter(is.numeric, non_na)
          imputed[r, c] <- non_na[sample(1:length(non_na), 1)]
        }
      }
    }
  }
  else {
    for(c in col.range) {
      for(r in row.range) {
        if(is.na(imputed[r, c])) {
          non_na <- imputed[which(is.na(imputed[, c]) != TRUE), c]
          imputed[r, c] <- non_na[sample(1:length(non_na), 1)]
        }
      }
    }
  }

  return(imputed)
}
