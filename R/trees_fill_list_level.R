#' @name trees_fill_list_level
#' @title Transform a tibble to a json
#' @author Nicolas Mangin
#' @description Function transforming a table containing a list of documents (with a position in a hierarchy) into a json file structure. Note that the classification assumes three roots: Included, Excluded and Unclassified.
#' @param root Tibble.
#' @param children Tibble.
#' @param text Character.
#' @return List interpreted as a JSON file by jsTreeR
#' @seealso trees_tibble_to_json
#' @seealso trees_create_list_level
#' @export


trees_fill_list_level <- function(
  root = NULL, children = NULL, text = NULL
){
  if (base::is.null(text)) text <- "tree"
  if (base::is.null(root)){
    if (base::is.null(children)){
      y <- base::list(
        text = text,
        icon = "fa fa-folder-open",
        data = base::list(
          file = NA,
          code = NA,
          language = NA,
          modified = NA,
          title = NA,
          type = NA,
          document = NA
        ),
        children = base::list()
      )
    } else {
      y <- base::list(
        text = text,
        icon = "fa fa-folder-open",
        data = base::list(
          file = NA,
          code = NA,
          language = NA,
          modified = NA,
          title = NA,
          type = NA,
          document = NA
        ),
        children = children
      )
    }
  } else {
    if (base::is.null(children)){
      y <- base::list(
        text = root$text[[1]],
        icon = base::paste0("fa fa-", root$icon[[1]]),
        data = base::list(
          file = root$file[[1]],
          code = root$code[[1]],
          language = root$language[[1]],
          modified = root$modified[[1]],
          title = root$title[[1]],
          type = root$type[[1]],
          document = root$document[[1]]
        ),
        children = base::list()
      )
    } else {
      y <- base::list(
        text = root$text,
        icon = base::paste0("fa fa-", root$icon[[1]]),
        data = base::list(
          file = root$file[[1]],
          code = root$code[[1]],
          language = root$language[[1]],
          modified = root$modified[[1]],
          title = root$title[[1]],
          type = root$type[[1]],
          document = root$document[[1]]
        ),
        children = children
      )
    }
  }
  return(y)
}

