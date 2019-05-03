
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
#' lite_df <- drop_vars_from_dataframe(df = df_example, formats_df = formats, post_dm = FALSE, drop_cmd = "drop")


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

