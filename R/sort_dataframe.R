
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

sort_dataframe <- function(df, formats_df = formats, post_dm = FALSE) {
  if (post_dm == FALSE) {
    formats_df <- formats_df %>%
      dplyr::filter(`Import format` != "not imported")
  }

  sort_vars <- formats_df %>%
    dplyr::filter(`Sorting Order`> 0) %>%
        dplyr::arrange(`Sorting Order`)  %>%
    dplyr::select(`Variable name`) %>%
    as.vector() %>%
    unname() %>%
    unlist()

df <- df %>%
  dplyr::select(sort_vars, dplyr::everything())
}
