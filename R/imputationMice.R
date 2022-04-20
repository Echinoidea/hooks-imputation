#' Multiple Imputation by Chained Equation
#'
#' Imputed missing data using \code{mice::mice()}. This function implements
#' some additional functionality to mice imputation to be performed by row
#' rather than by column.
#'
#' @param data a matrix-like R object with two dimensions containing the data to
#' be passed into \code{mice()}.
#' @param by.row a logical value indicating whether the data should be imputed
#' by row rather than by column (default).
#' @param row.range a numeric vector containing the row numbers to be included
#' in the MICE algorithm.
#' @param col.range a numeric vector containing the column numbers to be
#' included in the MICE algorithm.
#' @param mice.method Can be either a single string, or a vector of strings with
#' length length(blocks), specifying the imputation method to be used for each
#' column in data. If specified as a single string, the same method will be
#' used for all blocks. The default imputation method (when no argument is
#' specified) depends on the measurement level of the target column, as
#' regulated by the defaultMethod argument. Columns that need not be imputed
#' have the empty method "". See details. (from mice documentation. See
#' \code{?mice} for all mice methods).
#' @param m Numeric value passed to \code{mice()}. Number of multiple
#' imputations. The default is m = 5.
#' @param mice.action A numeric vector or a keyword to be passed into
#' \code{mice::complete()}. Numeric values between 1 and data$m return the data
#' with imputation number action filled in. The value of action = 0 return the
#' original data, with missing values. action can also be one of the following
#' keywords: "all", "long", "broad" and "repeated". See the Details section for
#' the interpretation. The default is action = 1L returns the first imputed data
#' set. (from mice documentation. See \code{?mice::complete}).
#' @param ... Additional arguments to be passed to \code{mice()}.
#'
#' @return Returns the passed data with missing values imputed.
#'
#' @examples
#' # default mice
#' imputed_data <- imputationMice(data)
#'
#' # mice performed by row, excluding the first column.
#' imputed_data_byrow <- imputationMice(data_byrow, by.row = TRUE,
#' col.range = 2:ncol(data_byrow))
#'
imputationMice <- function(data, by.row = FALSE, row.range = 1:nrow(data),
                           col.range = 1:ncol(data), mice.method = "pmm", m = 5,
                           mice.action = 1L, ...) {
  require(mice)

  imputed <- data[row.range, col.range]

  # MICE makes predictions by columns only. To perform MICE by rows, the data
  # must be transposed and formatted first.
  if (by.row) {
    imputed <- data.frame(t(imputed))
  }

  temp_data <- do.call("mice",
                       list(imputed,
                            meth = mice.method,
                            m = substitute(m),
                            ...
                       )
  )

  imputed <- mice::complete(temp_data, mice.action)

  # Transpose the data back to its original orientation.
  if (by.row) {
    imputed <- data.frame(t(imputed))
  }

  return(imputed)
}
