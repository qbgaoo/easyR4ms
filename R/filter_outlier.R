filter_outlier <- function(formula, data, coef = 1.5){
  df <- data
  x <- all.vars(formula[[2]])
  id <- all.vars(formula[[3]])
  k <- coef

  Q1 <- quantile(df[[x]], 0.25, na.rm = TRUE)
  Q3 <- quantile(df[[x]], 0.75, na.rm = TRUE)
  range <- Q3 - Q1
  lwr_bound <- Q1 - coef * range
  upr_bound <- Q3 + coef * range

  outliers <- df[df[[x]] < lwr_bound | df[[x]] > upr_bound,]

  if (nrow(outliers) > 0) {
    return(tibble(outliers))
  } else {
    return(print("No outliers found"))
  }
}

