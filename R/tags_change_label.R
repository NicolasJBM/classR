#' @name tags_change_label
#' @title Change the label of a tag
#' @author Nicolas Mangin
#' @description Function changing the label of a tag in all documents.
#' @param course_paths Reactive. Function containing a list of paths to the different folders and databases on local disk.
#' @param old_label Character. Initial label to be replaced.
#' @param new_label Character. New label replacing the previous one
#' @return Apply tag label change in the tag list and in all the documents.
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_replace
#' @export



tags_change_label <- function(course_paths, old_label, new_label){
  
  base::load(course_paths()$databases$tags)
  tags$tag <- stringr::str_replace_all(tags$tag, old_label, new_label)
  base::save(tags, file = course_paths()$databases$tags)
  
  base::load(course_paths()$databases$documents)
  base::names(documents) <- base::names(documents) |>
    stringr::str_replace_all(old_label, new_label)
  base::save(documents, file = course_paths()$databases$documents)
  
  for (file in documents$file){
    path <- base::paste0(course_paths()$subfolders$original, "/", file)
    lines <- base::readLines(path)
    untouched <- lines[1:(base::match('Meta-information', lines)+1)]
    metainfo <- lines[(base::match('Meta-information', lines)+2):(base::length(lines))]
    metainfo <- metainfo[base::nchar(metainfo)>0]
    newmetainfo <- stringr::str_replace(
      metainfo,
      base::paste0("exextra\\[", old_label,"\\]"),
      base::paste0("exextra\\[", new_label,"\\]")
    )
    newlines <- c(untouched, newmetainfo, "")
    base::writeLines(newlines, path, useBytes = TRUE)
  }
}
