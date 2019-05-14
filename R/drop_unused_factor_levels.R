
#' Drop unused factor levels for dataframe
#'
#' Drop unused factor levels from all factor variables in the dataframe.
#'
#' @param df dataframe
#' @param var_selection Character vector of selected variables to be set missing. Default \code{var_selection = c("all")} means that
#'                      all variables are selected
#'
#' @return dataframe
#' @export


drop_unused_factor_levels <- function(df, var_selection = c("_all")) {

  #drop factor levels
  if(var_selection == "_all"){
  df %>% df %>%
    dplyr::mutate_if(is.factor, as.factor)
  } else {
    df %>% df %>%
      dplyr::mutate_at(var_selection, as.factor)
  }

}
