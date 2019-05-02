
#' Import Formats from .xlsx
#'
#' Import formats from a predefined xlsx file.
#'
#' @param infile_formats file
#' @param sheet string
#'
#' @return dataframe
#' @export


read_formats_xlsx <- function(infile_formats, sheet = NULL) {
  formats <- readxl::read_xlsx(path = infile_formats,
                               sheet = sheet)

  #create columns for factor transformation. "var_values" and "var_valnames".
  a <- formats %>%
    dplyr::select(dplyr::contains("Value_label_")) %>%
    colnames() %>%
    length()

  for(i in 1:a) {
    formats <- formats %>%
      tidyr::separate(paste0("Value_label_", i, sep=""), into = c(paste0("var_value",i,sep=""),paste0("var_valname",i,sep="")), sep = "\\=", remove=FALSE)
  }
  return(formats)

}

