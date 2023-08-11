#' @name trees_selected_json_to_tibble
#' @title Tree selection as tibble
#' @author Nicolas Mangin
#' @description Function transforming a selection of documents from jsTreeR to a table
#' @param selected_tree List. Input from a jsTreeR output.
#' @return Tibble. List of selected documents.
#' @importFrom dplyr all_of
#' @importFrom dplyr mutate
#' @importFrom purrr map
#' @importFrom tibble tibble
#' @importFrom tidyr unnest
#' @export


trees_selected_json_to_tibble <- function(selected_tree){
  datalenght <- NULL
  selection <- tibble::tibble(selection = selected_tree) |>
    dplyr::mutate(selection = purrr::map(selection, function(x){
      y <- x$data
      if (base::length(y) > 0){
        y <- base::t(y)
        y <- base::as.data.frame(y)
      } else y <- NULL
      return(y)
    })) |>
    dplyr::mutate(datalenght = purrr::map_int(selection, base::length)) |>
    dplyr::filter(datalenght > 1) |>
    tidyr::unnest(selection)
  variables <- base::names(selection)
  selection <- tidyr::unnest(selection, dplyr::all_of(variables))
  return(selection)
}

