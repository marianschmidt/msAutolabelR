
#' Apply variable labels to dataframe
#'
#' Apply variable labels as defined in formats file to dataframe.
#'
#' @param df dataframe
#' @param formats_df dataframe defining the formats. Must be in standard format containing columns \code{Variable_name}, \code{Import_format}
#'                   and \code{Sorting_order}.
#' @param post_dm Logical; if \code{FALSE}, all \code{variables} are added as part of data management steps
#'                defined as \code{"not imported"} in column \code{Import_format} of the formats file will be
#'                omitted.
#'
#' @return dataframe
#' @export


label_vars_dataframe <- function(df, formats_df = formats, post_dm = FALSE) {

  #delete drop vars and not imported vars from format file used for labelling
  formats_label <- formats_df %>%
    dplyr::filter(is.na(Drop_from_analysis_file))

  #for pre datamanagement steps (post_dm=FALSE) new variables (not imported) do not exist yet and are omitted
  if (post_dm == FALSE) {
  formats_label <- formats_label  %>%
    dplyr::filter(Import_format != "not imported")
  }


  for(i in 1:nrow(formats_label)){
    df <- df %>% sjlabelled::var_labels(!!formats_label$`Variable_name`[i] := !!formats_label$`Variable_label`[i])
  }
  return(df)

}
