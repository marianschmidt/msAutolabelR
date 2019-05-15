
#' Change formats of columns in dataframe
#'
#' Apply column formats as defined in formats file to dataframe.
#'
#' @param df dataframe
#' @param formats_df dataframe defining the formats. Must be in standard format containing columns \code{Variable_name}, \code{Import_format}
#'                   and \code{Sorting_order}.
#' @param char_vars Logical if \code{TRUE}, all \code{variables} that are
#'                defined as \code{"String"} in column \code{Variable_type} of the formats file will be
#'                changed to \code{as.character}.
#' @param factor_vars Logical if \code{TRUE}, all \code{variables} that are
#'                defined as \code{"Labelled num"} in column \code{Variable_type} and
#'                \code{"Ordinal"} or \code{"Nominal"} in column \code{Measurement_level} of the formats file will be
#'                changed to \code{sjlabelled::as_label, keep.labels = TRUE}.
#' @param post_dm Logical; if \code{FALSE}, all \code{variables} are added as part of data management steps
#'                defined as \code{"not imported"} in column \code{Import_format} of the formats file will be
#'                omitted.
#' @param var_selection Character vector of selected variables will be changed. Default \code{var_selection = c("all")} means that
#'                      all variables are selected.
#'
#' @return dataframe
#' @export


change_formats_dataframe <- function(df, formats_df = formats, char_vars = FALSE, factor_vars = FALSE, post_dm = FALSE, var_selection = c("_all")) {

  #delete drop vars and not imported vars from format file used for labelling
  formats_label <- formats_df %>%
    dplyr::filter(is.na(Drop_from_analysis_file))

  #for pre datamanagement steps (post_dm=FALSE) new variables (not imported) do not exist yet and are omitted
  if (post_dm == FALSE) {
    formats_label <- formats_label  %>%
      dplyr::filter(Import_format != "not imported")
  }

  ###4 cond - limit variables on option var_selection
  if (var_selection != "_all") {

    #give warning in case variables defined in var_selection do not exist
    not_found <- var_selection[!(var_selection %in% formats_label$Variable_name)]
    warning("The following variables defined in var_selection are not found in the formats file: ", not_found)

    formats_label <- formats_label  %>%
      dplyr::filter(Variable_name %in% var_selection)

  }

  #make character variables
  if (char_vars == TRUE) {
    char_vars <- formats_label %>%
      dplyr::filter(Variable_type=="String") %>%
      dplyr::select(Variable_name) %>%
      as.vector() %>%
      unname() %>%
      unlist()

    #apply change to character var
    df <- df %>%
      dplyr::mutate_at(dplyr::vars(char_vars), as.character)
  }

  #make labelled nums factor variables
  if (factor_vars == TRUE) {
    factor_vars <- formats_label %>%
      dplyr::filter(Variable_type=="Labelled num" & (Measurement_level=="Nominal" | Measurement_level=="Ordinal")) %>%
      dplyr::select(Variable_name) %>%
      as.vector() %>%
      unname() %>%
      unlist()

    #apply change to factor var (you can reverse this by using sjlabelled::as_numeric(variable_name, use.labels = TRUE))
    df <- df %>%
      dplyr::mutate_at(dplyr::vars(factor_vars), sjlabelled::as_label, keep.labels = TRUE)
  }

   return(df)

}
