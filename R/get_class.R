#' Get class function
#'
#' @param R object from which you want to determin the classes, can be a list of objects or a data frame
#'
#' @return dataframe - in case a list is provided as input for every element in the list the function returns its class,
#' in case dataframe is an input the class is return for every column.
#' @export
get_classes <- function(x){
  purrr::map(x, class) %>%
    dplyr::as_data_frame() %>%
    tidyr::gather(name, class)
}
