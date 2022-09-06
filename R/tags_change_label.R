#' @name tags_change_label
#' @title Change the label of a tag
#' @author Nicolas Mangin
#' @description Function changing the label of a tag in all documents.
#' @param tags Tibble. List of tags, their values and attributes.
#' @param documents Tibble. List of documents
#' @param old_label Character. Initial label to be replaced.
#' @param new_label Character. New label replacing the previous one
#' @return Apply tag label change in the tag list and in all the documents.
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_replace
#' @export



tags_change_label <- function(tags, documents, old_label, new_label){
  
  tags$tag <- stringr::str_replace_all(
    tags$tag, old_label, new_label
  )
  base::save(tags, file = "2_documents/tags.RData")
  
  documents <- documents[!base::is.na(documents[,old_label]),]
  documents <- documents[documents[,old_label] != "",]
  
  for (path in documents$path){
    
    lines <- base::readLines(path)
    untouched <- lines[1:(base::match('Meta-information', lines)+1)]
    
    metainfo <- lines[(base::match('Meta-information', lines)+2):(base::length(lines))]
    
    newmetainfo <- stringr::str_replace(
      metainfo,
      base::paste0("exextra\\[", old_label,"\\]"),
      base::paste0("exextra\\[", new_label,"\\]")
    )
    
    newlines <- c(untouched, newmetainfo)
    
    base::writeLines(newlines, path, useBytes = TRUE)
  }
}
