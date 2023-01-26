#' @name tree_find_higher_level
#' @title Find the higher level folder
#' @author Nicolas Mangin
#' @description Function returning the required higher-level position of a document in a tree.
#' @param x Character. Level of the document.
#' @param y Character. Higher-level position which should be returned.
#' @return Character. Higher-level position
#' @importFrom stringr str_split
#' @export



tree_find_higher_level <- function(x, y){
  z <- x |>
    stringr::str_split("\\.", simplify = TRUE) |>
    base::as.numeric()
  w <- base::min(y, base::length(z)-1)
  z[c((w+1):base::length(z))] <- 0
  base::paste(z, collapse = ".")
}
