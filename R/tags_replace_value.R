#' @name tags_replace_value
#' @title Replace tag value
#' @author Nicolas Mangin
#' @description Function replacing the value of a tag by another in all documents.
#' @param tags Tibble. List of tags, their values and attributes.
#' @param documents Tibble. List of documents
#' @param tag_label Character. Label of the tag in which the value should be replaced.
#' @param old_value Character. Value which should be replaced.
#' @param new_value Character. Value replacing the old value (should exist in the tag list before).
#' @return Apply tag value change in all documents.
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace
#' @export


tags_replace_value <- function(tags, documents, tag_label, old_value, new_value){
  
  documents <- documents[documents[,tag_label] == old_value,]
  
  for (path in documents$path){
    lines <- base::readLines(path)
    untouched <- lines[1:(base::match('Meta-information', lines)+1)]
    metainfo <- lines[(base::match('Meta-information', lines)+2):(base::length(lines))]
    
    newmetainfo <- metainfo
    for (i in base::seq_len(base::length(metainfo))){
      if (stringr::str_detect(newmetainfo[i], tag_label)){
        newmetainfo[i] <- stringr::str_replace(
          newmetainfo[i], old_value, new_value
        )
      }
    }
    
    newlines <- c(untouched, newmetainfo)
    base::writeLines(newlines, path, useBytes = TRUE)
  }
  
}
