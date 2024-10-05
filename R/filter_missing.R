filter_missing <- function(conn, data){
  df <- data
  x <- all.vars(conn[[2]])
  id <- all.vars(conn[[3]])

  df <- df[is.na(df[[x]]),]
  miss_vl <- cbind(df[id], df[x])

  if (nrow(miss_vl) > 0) {
    return(miss_vl)
  } else {
    return(print("No missing value found"))
  }
}
