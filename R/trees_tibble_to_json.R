#' @name trees_tibble_to_json
#' @title Transform a tibble to a json
#' @author Nicolas Mangin
#' @description Function transforming a table containing a list of documents (with a position in a hierarchy) into a json file structure. Note that the classification assumes three roots: Included, Excluded and Unclassified.
#' @param tree_tibble Tibble. Classification tree for documents.
#' @return List interpreted as a JSON file by jsTreeR
#' @seealso trees_create_list_levels
#' @seealso trees_fill_list_level
#' @importFrom dplyr mutate_if
#' @importFrom stringr str_count
#' @importFrom stringr str_extract
#' @importFrom tidyr separate
#' @export


trees_tibble_to_json <- function(tree_tibble){

  position <- NULL

  # Count the number of levels of the hierarchy
  if (base::nrow(tree_tibble) > 0){
    depth <- stringr::str_count(tree_tibble$position[[1]], "\\.")+1
  } else {
    depth <- 1
  }

  # Identify the non-empty lists
  filled_folders <- base::unique(
    stringr::str_extract(tree_tibble$position, "^.")
  )
  tree_json <- tree_tibble |>
    tidyr::separate(position, into = base::paste0("LEV", 1:depth), sep = "\\.")
  tree_json <- tree_json |>
    dplyr::mutate_if(
      base::startsWith(base::names(tree_json), "LEV"),
      as.numeric
    ) |>
    classR::trees_create_list_levels(depth = depth)

  # This is a necessary slight reformating for the object to be an
  # appropriate node in jsTreeR. In addition, handles the case of missing folders
  if (filled_folders[1] == "3"){
    tree_json <- base::list(
      classR::trees_fill_list_level(text = "Included"),
      classR::trees_fill_list_level(text = "Excluded"),
      classR::trees_fill_list_level(
        children = tree_json$children[[1]], text = "Unclassified"
      )
    )
  } else {
    tree_json <- base::list(
      tree_json$children[[1]],
      tree_json$children[[2]],
      tree_json$children[[3]]
    )
  }

  return(tree_json)
}
