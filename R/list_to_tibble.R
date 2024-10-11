#' Convert list-column data frame (including different lengths) to a common one
#'
#' @param x A list-columns
#'
#' @return A tibble data frame
#' @export
#'
#' @importFrom purrr map pluck set_names
#' @importFrom tibble as_tibble
#'
#' @examples
#' library(tibble)
#' x <- tibble(sex = list(c("F", "M", "M")), age = list(c(22, 20, 23)))
#' list_to_tibble(x)
list_to_tibble <- function (x) {
  tbl <- x
  max_nrow <- tbl |>
    sapply(pluck) |>
    map(length) |>
    unlist(use.names = F) |>
    max()
  n_col <- ncol(tbl)

  new_tbl <- matrix(
    nrow = max_nrow,
    ncol = n_col
  ) |>
    as_tibble(.name_repair = "minimal") |>
    set_names(nm = names(tbl))

  for(i in 1:n_col){
    element_nrow <- tbl |>
      pluck(i) |>
      unlist() |>
      length()
    len_diff = max_nrow - element_nrow

    if(len_diff > 0){
      new_tbl[,i] <- tbl |>
        pluck(i) |>
        unlist() |>
        c(rep(NA, len_diff))
    } else{
      new_tbl[,i] <- tbl  |>
        pluck(i) |>
        unlist()
    }
  }
  return (new_tbl)
}

