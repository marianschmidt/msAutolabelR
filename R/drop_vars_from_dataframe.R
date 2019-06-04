
#' Drop Variables From Dataframe
#'
#' Drop the variables defined in the format table from a specific dataframe.
#'
#' @param df dataframe
#' @param formats_df dataframe defining the formats. Must be in standard format containing columns \code{Variable_name}, \code{Drop_from_analysis_file}
#'                   and \code{Sorting_order}.
#' @param post_dm Logical; if \code{FALSE}, all \code{variables} are added as part of data management steps
#'                defined as \code{"not imported"} in column \code{Import_format} of the formats file will be
#'                omitted.
#'
#' @param drop_cmd String; defines the character string in column \code{Drop_from_analysis_file} of the formats table
#'                 to indicate that the variable should be dropped. By default  \code{drop_cmd = "drop"}
#'
#' @return dataframe
#'
#' @export



drop_vars_from_dataframe <- function(df, formats_df = formats, post_dm = FALSE, drop_cmd = "drop") {

  #for pre datamanagement steps (post_dm=FALSE) new variables (not imported) do not exist yet and are omitted
  if (post_dm == FALSE) {
    formats_df <- formats_df %>%
      dplyr::filter(Import_format != "not imported")
  }

  drop_vars <- formats_df %>%
    dplyr::filter(Drop_from_analysis_file == drop_cmd) %>%
    dplyr::select(Variable_name) %>%
    as.vector() %>%
    unname() %>%
    unlist()

  df %>%
    dplyr::select(-drop_vars)
}

