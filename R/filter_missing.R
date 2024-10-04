filter_missing <- function(formula, data){
  df <- data
  x <- all.vars(formula[[2]])
  id <- all.vars(formula[[3]])

  miss_vl <- df[is.na(df[[x]]),]

  if (n > 0) {
    return(tibble(miss_vl))
  } else {
    return(print("No missing value found"))
  }
}
