missingDataPercentage <- function(x, col.range = 1:ncol(x)) {
  mp <- data.frame(matrix(nrow = 1, ncol = length(col.range)))

  for(i in col.range) {
    mp[1,i] <- sum(is.na(x[, i])) / nrow(x) * 100
  }

  colnames(mp) <- colnames(x)

  return(mp)
}
