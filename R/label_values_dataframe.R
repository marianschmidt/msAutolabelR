
#' Apply value labels to dataframe
#'
#' Apply value labels as defined in formats file to all variables in the dataframe.
#'
#' @param df dataframe
#' @param formats_df dataframe defining the formats. Must be in standard format containing columns \code{Variable_name}, \code{Value_Label_1-x}.
#'                   If needed run function \code{read_formats_xlsx.R} beforehand.
#' @param post_dm Logical; if \code{FALSE}, all \code{variables} are added as part of data management steps
#'                defined as \code{"not imported"} in column \code{Import_format} of the formats file will be
#'                omitted.
#'
#' @return dataframe
#' @export


label_values_dataframe <- function(df, formats_df = formats, post_dm = FALSE) {

  #delete drop vars and not imported vars from format file used for labelling
  formats_label <- formats_df %>%
    dplyr::filter(is.na(Drop_from_analysis_file))

  #for pre datamanagement steps (post_dm=FALSE) new variables (not imported) do not exist yet and are omitted
  if (post_dm == FALSE) {
    formats_label <- formats_label  %>%
      dplyr::filter(Import_format != "not imported")
  }


  #apply value labels
  for(i in 1:nrow(formats_label)){
    if(!is.na(formats_label$var_value1[i])){

      sj_vals <- formats_label %>%
        dplyr::select(contains("var_value")) %>%
        dplyr::filter(row_number() == i) %>%
        dplyr::select_if(function(x) !(all(is.na(x)))) %>%
        as.numeric %>%
        unlist()

      sj_labs <- formats_label %>%
        dplyr::select(contains("var_valname")) %>%
        dplyr::filter(row_number() == i) %>%
        dplyr::select_if(function(x) !(all(is.na(x)))) %>%
        unlist()

      if(length(sj_vals)>0 & length(sj_vals)==length(sj_labs)){

        sjlabels <- setNames(sj_vals, sj_labs)

        df <- df %>%
          sjlabelled::val_labels(!!formats_label$`variable_name`[i] := !!sjlabels)
      }
    }
  }

  return(df)

}
