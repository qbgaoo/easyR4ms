#' Filter the outlies with IDs
#'
#' @param conn A expression like x ~ id, x is the numerical variable.
#' @param data A data frame, data frame extension (e.g. a tibble).
#' @param coef A numerical value. Defaults to 1.5.
#'
#' @return A data frame
#' @export
#' @importFrom stats na.omit quantile
#' @examples
#' conn <- x ~ id
#' data <- data.frame(
#'   id = c(1:6),
#'   x = c(5.2, 4.5, 3.9, 30, 4.5, 4.3))
#' filter_outlier(conn, data)
#' filter_outlier(conn, data, coef = 1.5)
filter_outlier <- function(conn, data, coef = 1.5){
  x <- all.vars(conn[[2]])
  id <- all.vars(conn[[3]])
  df <- data[c(id, x)]
  df <- na.omit(df)

  q1 <- quantile(df[[x]], 0.25)
  q3 <- quantile(df[[x]], 0.75)
  lwr_bound <- q1 - coef * (q3 - q1)
  upr_bound <- q3 + coef * (q3 - q1)

  outliers <- df[df[[x]] < lwr_bound | df[[x]] > upr_bound,]
  if (nrow(outliers) > 0) {
    return(outliers)
  } else {
    return("No outlier found")
  }
}
