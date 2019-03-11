
#' Sort Dataframe
#'
#' Sort an existing dataset according to the order of variables defined in the format table.
#'
#' @param df dataframe
#'
#' @return dataframe
#' @export
#' @examples
#' sort_dataframe(iris)

sort_dataframe <- function(df) {
  sort_vars <- formats %>%
    dplyr::filter(`Sorting Order`> 0) %>%
    dplyr::arrange(`Sorting Order`)  %>%
    dplyr::select(`Variable name`) %>%
    as.vector() %>%
    unname() %>%
    unlist()

df <- df %>%
  dplyr::select(sort_vars, everything())
}
