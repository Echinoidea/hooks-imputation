# Custom expectation for if an object contains NA values
expect_na <- function(object) {
  # 1. Capture object and label
  act <- quasi_label(rlang::enquo(object))

  if(any(apply(act$val, 2, function(x) any(is.na(x))) == TRUE)) {
    succeed()
    invisible(act$val)
  }
  else {
    fail()
    invisible(act$val)
  }
}
