#' Create a 3 line flextable
#'
#' @param x A flextable object.
#'
#' @return A flextable object.
#' @export
#' @importFrom officer fp_border
#' @importFrom flextable border_remove hline hline_top hline_bottom
#' @importFrom flextable line_spacing set_table_properties align
#'
#' @examples
#' library(flextable)
#' x <- data.frame(
#'   sex = c("F", "M", "F"),
#'   age = c(22, 20, 30))
#' x <- flextable(x)
#' flex_3line_table(x)
flex_3line_table  <-  function(x){
  flex_tbl <- x
  thick_border <- fp_border(color = "black", width = 2)
  thin_border  <- fp_border(color = "black", width = 1)

  flex_tbl <- flex_tbl |>
    border_remove() |>
    hline_top(border = thick_border, part = "header") |>
    hline(i = 1, border = thin_border, part = "header") |>
    hline_bottom(border = thick_border) |>
    line_spacing(space = 0.5, part = "body") |>
    set_table_properties(width = 0.9, layout = "autofit") |>
    align(align = "center", part = "all")

  flex_tbl
}
