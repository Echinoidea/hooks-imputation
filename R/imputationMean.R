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
