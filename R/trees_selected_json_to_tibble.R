#' @name trees_selected_json_to_tibble
#' @title Tree selection as tibble
#' @author Nicolas Mangin
#' @description Function transforming a selection of documents from jsTreeR to a table
#' @param selected_tree List. input from a jsTreeR output.
#' @return Tibble. List of selected documents.
#' @importFrom tibble tibble
#' @importFrom dplyr mutate
#' @importFrom purrr map
#' @importFrom tidyr unnest
#' @importFrom tidyr unnest
#' @importFrom dplyr all_of
#' @export


trees_selected_json_to_tibble <- function(selected_tree){
  selection <- tibble::tibble(selection = selected_tree) |>
    dplyr::mutate(selection = purrr::map(selection, function(x){
      base::as.data.frame(base::t(x$data))
    })) |>
    tidyr::unnest(selection)
  variables <- base::names(selection)
  selection <- tidyr::unnest(selection, dplyr::all_of(variables))
  return(selection)
}

