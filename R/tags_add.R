#' @name tags_add
#' @title Add a tag
#' @author Nicolas Mangin
#' @description Function adding a tag to all documents.
#' @param tags Tibble. List of tags, their values and attributes.
#' @param documents Tibble. List of documents
#' @param tag_label Character. Label of the tag which should be added.
#' @return Add an empty tag in all documents.
#' @export


tags_add <- function(tags, documents, tag_label){
  for (path in documents$path){
    lines <- base::readLines(path)
    untouched <- lines[1:(base::match('Meta-information', lines)+1)]
    metainfo <- lines[(base::match('Meta-information', lines)+2):(base::length(lines))]
    newmetainfo <- c(
      metainfo,
      base::paste0("exextra[", tag_label,"]:")
    )
    newlines <- c(untouched, newmetainfo)
    base::writeLines(newlines, path, useBytes = TRUE)
  }
}

