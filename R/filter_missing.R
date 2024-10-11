#' Filter the missing values with IDs
#'
#' @param conn A expression like x ~ id, x is the numerical variable.
#' @param data A data frame, data frame extension (e.g. a tibble).
#'
#' @return A data frame
#' @export
#'
#' @examples
#' conn <- x ~ id
#' data <- data.frame(
#'   id = c(1:6),
#'   x = c(5.2, NA, 3.9, NA, 4.5, 4.3)
#' )
#' filter_missing(conn, data)
filter_missing <- function(conn, data){
  x <- all.vars(conn[[2]])
  id <- all.vars(conn[[3]])

  df_missing <- data[is.na(data[[x]]), c(id, x)]


  if (nrow(df_missing) > 0) {
    return(df_missing)
  } else {
    return("No missing value found")
  }
}

