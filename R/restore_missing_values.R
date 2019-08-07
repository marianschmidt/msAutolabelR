
#' Restore missing values for dataframe
#'
#' Restore missing values (numeric instead of NA) as defined in formats file for all variables in the dataframe.
#'
#' @param df dataframe
#' @param formats_df dataframe defining the formats. Must be in standard format containing columns \code{Variable_name}, \code{Value_Label_1-x}.
#'                   If needed run function \code{read_formats_xlsx.R} beforehand.
#' @param post_dm Logical; if \code{FALSE}, all \code{variables} are added as part of data management steps
#'                defined as \code{"not imported"} in column \code{Import_format} of the formats file will be
#'                omitted.
#' @param omit_labelled Logical; if \code{TRUE}, all \code{variables} that are already labelled, will be omitted.
#'                    Default is \code{omit_tagged = FALSE}, so that all previously labelled variables will be tagged again.
#' @param var_selection Character vector of selected variables to be set missing. Default \code{var_selection = c("all")} means that
#'                      all variables are selected.
#'
#' @return dataframe
#' @export


restore_missing_values <- function(df, formats_df = formats, post_dm = FALSE, omit_labelled = FALSE, var_selection = c("_all")) {

  ###1 delete drop vars and not imported vars from format file used for labelling
  formats_label <- formats_df %>%
    dplyr::filter(is.na(.data$Drop_from_analysis_file))

  ###2 cond - for pre datamanagement steps (post_dm=FALSE) new variables (not imported) do not exist yet and are omitted
  if (post_dm == FALSE) {
    formats_label <- formats_label  %>%
      dplyr::filter(.data$Import_format != "not imported")
  }

  ###3 cond - if (omit_labelled=TRUE) previously labelled variables are omitted
  if (omit_labelled == TRUE) {

    #function to find out if value labels have been previously applied
    has_no_label <- function(x) {
      if(length(attr(x, "labels")) > 0) {
        return(FALSE)
      } else {
        return(TRUE)
      }
    }

    #get names of labelled variables in df
    prev_not_labelled <- df %>%
      dplyr::select_if(has_no_label) %>%
      colnames()
    prev_labelled <- df %>%
      dplyr::select_if(has_no_label) %>%
      colnames()

    #give warning in case combination of omit_labelled and var_selection creates a problem
    problematic_vars <- var_selection[(var_selection %in% prev_labelled)]
    if(all(var_selection != "_all") && (length(problematic_vars) > 0)) {

      warning("The following variables defined in var_selection have been previously labelled. There is a conflict between the omit_lablled=TRUE option and var_selection: ", problematic_vars)
    }

    #limit formats to variables that are in list of not labelled
    formats_label <- formats_label  %>%
      dplyr::filter(.data$Variable_name %in% prev_not_labelled)
  }

  ###4 cond - limit variables on option var_selection
  if(all(var_selection != "_all")) {

    #give warning in case variables defined in var_selection do not exist

    not_found <- var_selection[!(var_selection %in% formats_label$Variable_name)]

    if(length(not_found) > 0) {
    warning("The following variables defined in var_selection are not found in the formats file: ", not_found)
    }

    formats_label <- formats_label  %>%
      dplyr::filter(.data$Variable_name %in% var_selection)

  }

  ###restore missing values
  for(i in 1:nrow(formats_label)){
    if(!is.na(formats_label$Missing_values[i])){

      sj_miss <- formats_label %>%
        dplyr::select(c("Missing_values")) %>%
        dplyr::filter(dplyr::row_number() == i) %>%
        dplyr::select_if(function(x) !(all(is.na(x)))) %>%
        unlist()

      if((!is.numeric(sj_miss)) & length(sj_miss)>0){
        sj_miss <- strsplit(sj_miss, split = ",") %>%
          unlist %>%
          as.numeric()
      }

      if(length(sj_miss)>0){

        #find value label for missing
        sj_miss_lab <- formats_label %>%
          dplyr::filter(dplyr::row_number() == i) %>%
          dplyr::select(dplyr::contains("Value_label_"))

        sj_miss_lab <- dplyr::select_if(sj_miss_lab, ~!is.na(.) & str_detect(., paste0("^", sj_miss)))

        sj_miss_lab <- sj_miss_lab %>%
          tidyr::separate(1, into = c("test1", "test2"), sep = "\\=", remove=FALSE) %>%
          dplyr::select("test2") %>%
          dplyr::pull()


      ##for Numeric Variables
      if(is.numeric(unlist(df[formats_label$Variable_name[i]]))) {

        #check if value has been used before (numeric value of defined missing)
        if(any(df[formats_label$Variable_name[i]] == sj_miss, na.rm = TRUE)){
        warning("The numeric value to be applied instead of NAs is already used. Check if it makes sense to restore missing values for this variable.")
        }

        #replace NAs with numeric values
        df <- df %>%
          dplyr::mutate_at(formats_label$Variable_name[i], ~tidyr::replace_na(., sj_miss))

      }

      ##for Factor Variables
      if(is.factor(unlist(df[formats_label$Variable_name[i]]))) {

        #check if value has been used before (factor value of defined missing)
        if(any(df[formats_label$Variable_name[i]] == sj_miss_lab, na.rm = TRUE)){
          warning("The value to be applied instead of NAs is already used. Check if it makes sense to restore missing values for this variable.")
        }

        #replace NAs with factor level
        df <- df %>%
          dplyr::mutate_at(formats_label$Variable_name[i], ~forcats::fct_expand(., !!sj_miss_lab)) %>%
          dplyr::mutate_at(formats_label$Variable_name[i], ~tidyr::replace_na(., !!sj_miss_lab))

      }

      }

      }
    }

  return(df)

}

