#' @name tags_remove
#' @title Remove a tag from all documents
#' @author Nicolas Mangin
#' @description Function removing a tag from all documents.
#' @param tags Tibble. List of tags, their values and attributes.
#' @param documents Tibble. List of documents
#' @param tag_label Character. Label of the tag which should be removed.
#' @return Remove the specified tag from all documents.
#' @importFrom dplyr filter
#' @importFrom stringr str_detect
#' @export


tags_remove <- function(tags, documents, tag_label){
  
  tags <- tags |>
    dplyr::filter(tag != tag_label)
  base::save(tags, file = "2_documents/tags.RData")
  
  documents[,tag_label] <- NULL
  base::save(documents, file = "2_documents/documents.RData")
  
  for (path in documents$path){
    lines <- base::readLines(path)
    untouched <- lines[1:(base::match('Meta-information', lines)+1)]
    metainfo <- lines[(base::match('Meta-information', lines)+2):(base::length(lines))]
    newmetainfo <- metainfo[!stringr::str_detect(
      metainfo, base::paste0("exextra\\[", tag_label,"\\]:")
    )]
    newlines <- c(untouched, newmetainfo)
    base::writeLines(newlines, path, useBytes = TRUE)
  }
  
}

