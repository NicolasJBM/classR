#' @name tags_replace_value
#' @title Replace tag value
#' @author Nicolas Mangin
#' @description Function replacing the value of a tag by another value in all documents. This is particularly useful when the user wants to change a label.
#' @param course_paths Reactive. Function containing a list of paths to the different folders and databases on local disk.
#' @param tag_label Character. Label of the tag in which the value should be replaced.
#' @param old_value Character. Value which should be replaced.
#' @param new_value Character. Value replacing the old value (should exist in the tag list before).
#' @return Apply tag value change in all documents.
#' @importFrom dplyr case_when
#' @importFrom dplyr mutate
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace
#' @importFrom stringr str_replace_all
#' @export


tags_replace_value <- function(course_paths, tag_label, old_value, new_value){
  
  base::load(course_paths()$databases$tags)
  tags <- tags |>
    dplyr::mutate(value = dplyr::case_when(
      tag == tag_label & value == old_value ~ new_value,
      TRUE ~ value
    ))
  base::save(tags, file = course_paths()$databases$tags)
  
  base::load(course_paths()$databases$documents)
  tagval <- base::as.character(documents[,tag_label])
  documents[,tag_label] <- stringr::str_replace_all(tagval, old_value, new_value)
  base::save(documents, file = course_paths()$databases$documents)
  
  for (file in documents$file){
    path <- base::paste0(course_paths()$subfolders$original, "/", file)
    lines <- base::readLines(path)
    untouched <- lines[1:(base::match('Meta-information', lines)+1)]
    metainfo <- lines[(base::match('Meta-information', lines)+2):(base::length(lines))]
    metainfo <- metainfo[base::nchar(metainfo)>0]
    newmetainfo <- metainfo
    for (i in base::seq_len(base::length(metainfo))){
      if (stringr::str_detect(newmetainfo[i], tag_label)){
        newmetainfo[i] <- stringr::str_replace(
          newmetainfo[i], old_value, new_value
        )
      }
    }
    newlines <- c(untouched, newmetainfo, "")
    base::writeLines(newlines, path, useBytes = TRUE)
  }
  
}
