#' @name tbltree_to_jstree
#' @title Transform a table tree to a json tree
#' @author Nicolas Mangin
#' @description Function transforming a table containing a list of documents (with a position in a hierarchy) into a json file structure. Note that the classification assumes three roots: Included, Excluded and Unclassified.
#' @param tbltree Tibble. Classification tree for documents.
#' @return List interpreted as a JSON file by jsTreeR
#' @importFrom dplyr arrange
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom stringr str_detect
#' @export

tbltree_to_jstree <- function(tbltree){
  
  path <- NULL
  
  dfs <- base::lapply(base::strsplit(tbltree$path, "/"), function(s){
    item <-
      base::Reduce(function(a,b) base::paste0(a,"/",b), s[-1], s[1], accumulate = TRUE)
    base::data.frame(
      item = item,
      parent = c("root", item[-base::length(item)]),
      stringsAsFactors = FALSE
    )
  })
  
  dat <- dfs[[1]] |>
    dplyr::mutate(order = 1, rank = base::row.names(dfs[[1]]))
  for(i in 2:length(dfs)){
    tmpdat <- dfs[[i]] |>
      dplyr::mutate(order = i, rank = base::row.names(dfs[[i]]))
    dat <- base::merge(dat, tmpdat, all = TRUE, sort = FALSE)
  }
  
  dat <- dat |>
    dplyr::arrange(order, rank) |>
    dplyr::select(-order, -rank) |>
    base::unique()
  
  f <- function(parent, tbltree){
    i <- base::match(parent, dat$item)
    item <- dat$item[i]
    children <- dat$item[dat$parent==item]
    label <- utils::tail(base::strsplit(item, "/")[[1]], 1)
    attr <- dplyr::filter(tbltree, stringr::str_detect(path, label))
    if(base::length(children) > 0){
      base::list(
        text = label,
        icon = "fa fa-folder-open",
        data = base::list(
          type = base::as.character(NA),
          translations = base::as.character(NA),
          file = base::as.character(NA)
        ),
        children = base::lapply(children, f, tbltree)
      )
    } else {
      if (base::is.na(attr$title[1])) text <- label else text <- attr$title[1]
      base::list(
        text = text,
        icon = base::paste0("fa fa-", attr$icon[1]),
        data = base::list(
          type = attr$type[[1]],
          translations = attr$translations[[1]],
          file = attr$file[[1]]
        )
      )
    }
  }
  base::lapply(dat$item[dat$parent == "root"], f, tbltree = tbltree)
}
