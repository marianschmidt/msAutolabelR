
#' Set missing values for dataframe
#'
#' Set missing values as defined in formats file to all variables in the dataframe.
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


set_missing_values <- function(df, formats_df = formats, post_dm = FALSE) {

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
    if(!is.na(formats_label$Missing_values[i])){

      sj_miss <- formats %>%
        dplyr::select(c("Missing_values")) %>%
        dplyr::filter(dplyr::row_number() == i) %>%
        dplyr::select_if(function(x) !(all(is.na(x)))) %>%
        unlist()

      if((!is.numeric(sj_miss)) & length(sj_miss)>0){
      sj_miss <- strsplit(sj_miss, split = ",") %>%
        unlist %>%
        as.numeric()
      }

      if(formats_label$Variable_type[i]=="Labelled num" & !is.numeric(unlist(delir[formats_label$Variable_name[i]]))){
        warning("Variable is not numeric anymore. Execute set_missing_values() BEFORE change_formats_dataframe()")
      }

      if(length(sj_miss)>0){

        df <- df %>%
          sjlabelled::set_na(!!formats_label$Variable_name[i], na = sj_miss, as.tag = TRUE)
      }
    }
  }

  return(df)

}
