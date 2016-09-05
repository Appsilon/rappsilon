#' json_to_list - function to convert a columncontaining json to a list in R
#'
#' @param data - dataframe
#' @param column_name - column name containg json
#'
#' @return list
json_to_list <- function (data, column_name){

  extract <- lapply(data[,column_name], function(x) {
    if(x != ""){
      as.list(jsonlite::fromJSON(x))
    }else{
      list(empty = NA)
    }})
}
