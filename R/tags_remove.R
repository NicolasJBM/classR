#' @name tags_remove
#' @title Remove a tag from all documents
#' @author Nicolas Mangin
#' @description Function removing a tag from all documents.
#' @param course_paths Reactive. Function containing a list of paths to the different folders and databases on local disk.
#' @param tag_label Character. Label of the tag which should be removed.
#' @return Remove the specified tag from all documents.
#' @importFrom dplyr all_of
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom stringr str_detect
#' @export


tags_remove <- function(course_paths, tag_label){
  
  base::load(course_paths()$databases$tags)
  tags <- dplyr::filter(tags, tag != tag_label)
  base::save(tags, file = course_paths()$databases$tags)
  
  base::load(course_paths()$databases$documents)
  
  if (dplyr::all_of(tag_label) %in% base::names(documents))
    documents <- dplyr::select(documents, -dplyr::all_of(tag_label))
  base::save(documents, file = course_paths()$databases$documents)
  
  for (file in documents$file){
    path <- base::paste0(course_paths()$subfolders$original, "/", file)
    lines <- base::readLines(path)
    untouched <- lines[1:(base::match('Meta-information', lines)+1)]
    metainfo <- lines[(base::match('Meta-information', lines)+2):(base::length(lines))]
    metainfo <- metainfo[base::nchar(metainfo)>0]
    newmetainfo <- metainfo[!stringr::str_detect(
      metainfo, base::paste0("exextra\\[", tag_label,"\\]:")
    )]
    newlines <- c(untouched, newmetainfo, "")
    base::writeLines(newlines, path, useBytes = TRUE)
  }
  
}

