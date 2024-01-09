
#' Title
#'
#' @return
#' @export
#'
#' @examples
get_raw_data <- function() {

  raw_data <- arrow::read_parquet("data-raw/raw-data.parquet")

  return(raw_data)
}
