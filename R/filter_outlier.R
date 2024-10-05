filter_outlier <- function(conn, data, coef = 1.5){
  x <- all.vars(conn[[2]])
  id <- all.vars(conn[[3]])
  k <- coef
  df <- cbind(df[id], df[x])
  df <- na.omit(df)


  Q1 <- quantile(df[[x]], 0.25)
  Q3 <- quantile(df[[x]], 0.75)
  range <- Q3 - Q1
  lwr_bound <- Q1 - k * range
  upr_bound <- Q3 + k * range

  outliers <- df[df[[x]] < lwr_bound | df[[x]] > upr_bound,]
  if (nrow(outliers) > 0) {
    return(outliers)
  } else {
    return(print("No outlier found"))
  }
}
