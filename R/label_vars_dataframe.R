
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
#' @param omit_labelled Logical; if \code{TRUE}, all \code{variables} that are already labelled, will be omitted.
#'                    Default is \code{omit_tagged = FALSE}, so that all previously labelled variables will be tagged again.
#'
#' @param var_selection Character vector of selected variables to be labelled. Default \code{var_selection = c("all")} means that
#'                      all variables are selected.
#' @return dataframe
#' @export


label_vars_dataframe <- function(df, formats_df = formats, post_dm = FALSE, omit_labelled = FALSE, var_selection = c("_all")) {

  ###1 delete drop vars and not imported vars from format file used for labelling
  formats_label <- formats_df %>%
    dplyr::filter(is.na(Drop_from_analysis_file))

  ###2 cond - for pre datamanagement steps (post_dm=FALSE) new variables (not imported) do not exist yet and are omitted
  if (post_dm == FALSE) {
    formats_label <- formats_label  %>%
      dplyr::filter(Import_format != "not imported")
  }

  ###3 cond - if (omit_labelled=TRUE) previously labelled variables are omitted
  if (omit_labelled == TRUE) {

    #function to find out if value labels have been previously applied
    has_no_label <- function(x) {
      if(length(attr(x, "label")) > 0) {
        return(FALSE)
      } else {
        return(TRUE)
      }
    }

    #get names of labelled variables in df
    prev_not_labelled <- df %>%
      select_if(has_no_label) %>%
      colnames()
    prev_labelled <- df %>%
      select_if(has_no_label) %>%
      colnames()

    #give warning in case combination of omit_labelled and var_selection creates a problem
    problematic_vars <- var_selection[(var_selection %in% prev_labelled)]
    warning("The following variables defined in var_selection have been previously labelled. There is a conflict between the omit_lablled=TRUE option and var_selection.", problematic_vars)

    #limit formats to variables that are in list of not labelled
    formats_label <- formats_label  %>%
      dplyr::filter(Variable_name %in% prev_not_labelled)
  }

  ###4 cond - limit variables on option var_selection
  if (var_selection != "_all") {

    #give warning in case variables defined in var_selection do not exist
    not_found <- var_selection[!(var_selection %in% formats_label$Variable_name)]
    warning("The following variables defined in var_selection are not found in the formats file: ", not_found)

    formats_label <- formats_label  %>%
      dplyr::filter(Variable_name %in% var_selection)

  }

  for(i in 1:nrow(formats_label)){
    df <- df %>% sjlabelled::var_labels(!!formats_label$`Variable_name`[i] := !!formats_label$`Variable_label`[i])
  }
  return(df)

}
