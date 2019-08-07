
#' Sort Dataframe
#'
#' Sort an existing dataset according to the order of variables defined in the format table.
#'
#' @param df dataframe
#' @param formats_df dataframe defining the formats. Must be in standard format containing columns \code{Variable_name}, \code{Import_format}
#'                   and \code{Sorting_order}.
#' @param post_dm Logical; if \code{FALSE}, all \code{variables} are added as part of data management steps
#'                defined as \code{"not imported"} in column \code{Import_format} of the formats file will be
#'                omitted.
#'
#' @return dataframe
#'
#' @export
#' @examples
#' #simple example dataframe
#' df_example <- data.frame(ID = c("10001", "10002", "10003"),
#'                          sex = c(0, 1, 2),
#'                          birth = c("2015-01-05", "2016-07-30", "2015-01-01"),
#'                          region = c("DE11", "DE12", "DE1X"),
#'                          region_num = c(11,12,19),
#'                          stringsAsFactors = FALSE)
#'
#' #example definitions of variable formats and labels
#' formats <- data.frame(Variable_name = c("ID", "sex", "birth", "region", "region_num"),
#'                       Variable_label = c("Patient ID", "Gender", "Date of birth [YYYY-MM-DD]",
#'                       "Region (NUTS-2 Code)", "Region [numeric]"),
#'                       Variable_type = c("String", "Labelled num", "Date", "String", "Labelled num"),
#'                       Sorting_order = c(1,3,2,4,5),
#'                       Import_format = c("chr", "num", "chr", "chr", "not imported"),
#'                       Drop_from_analysis_file = c(NA, NA, NA, "drop", NA),
#'                       Missing_values = c(NA, NA, NA, NA, "19"),
#'                       Value_labels = c(NA, "yes", NA, NA, "yes"),
#'                       stringsAsFactors = FALSE)
#'
#' sorted_df <- sort_dataframe(df = df_example, formats_df = formats, post_dm = FALSE)



sort_dataframe <- function(df, formats_df = formats, post_dm = FALSE) {

  #delete drop vars and not imported vars from format file used for labelling
  formats_label <- formats_df %>%
    dplyr::filter(is.na(Drop_from_analysis_file))

  #for pre datamanagement steps (post_dm=FALSE) new variables (not imported) do not exist yet and are omitted
  if (post_dm == FALSE) {
    formats_label <- formats_label %>%
      dplyr::filter(Import_format != "not imported")
  }

  sort_vars <- formats_label %>%
    dplyr::filter(Sorting_order > 0) %>%
    dplyr::arrange(Sorting_order)  %>%
    dplyr::select(Variable_name) %>%
    as.vector() %>%
    unname() %>%
    unlist()

  #ignore additional variables in format file that cannot be found in df and just return message about it. Still sort everything.
  not_found <- sort_vars[!(sort_vars %in% colnames(df))]

  message(paste0("The following variables in the formats file are not present in the df and are therefore omitted from sorting: ",
                paste(not_found, collapse = ", ")))

  sort_vars <- sort_vars[sort_vars %in% colnames(df)]

  df %>%
    dplyr::select(sort_vars, dplyr::everything())
}
