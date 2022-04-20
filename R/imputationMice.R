imputationMice <- function(data, by.row = FALSE, row.range = 1:nrow(data),
                           col.range = 1:ncol(data), mice.method = "pmm", m = 5,
                           maxit = 50, mice.action = 1L, ...) {
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
                            maxit = substitute(maxit),
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
