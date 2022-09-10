#' @name tags_add
#' @title Add a tag
#' @author Nicolas Mangin
#' @description Function adding a tag to all documents.
#' @param course_paths Reactive. Function containing a list of paths to the different folders and databases on local disk.
#' @param tag_label Character. Label of the tag which should be removed.
#' @return Add an empty tag in all documents.
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom tibble tibble
#' @export


tags_add <- function(course_paths, tag_label){
  
  base::load(course_paths()$databases$tags)
  add <- tibble::tibble(
      tag = tag_label,
      order = 0,
      value = "",
      count = 0,
      icon = "triangle-exclamation",
      boxcolor = "black"
    )
  tags <- dplyr::bind_rows(tags, add) |>
    dplyr::arrange(tag, order)
  base::save(tags, file = course_paths()$databases$tags)
  
  base::load(course_paths()$databases$documents)
  documents[,tag_label] <- base::as.character(NA)
  base::save(documents, file = course_paths()$databases$documents)
  
  for (file in documents$file){
    path <- base::paste0(course_paths()$subfolders$original, "/", file)
    lines <- base::readLines(path)
    untouched <- lines[1:(base::match('Meta-information', lines)+1)]
    metainfo <- lines[(base::match('Meta-information', lines)+2):(base::length(lines))]
    metainfo <- metainfo[base::nchar(metainfo)>0]
    newmetainfo <- c(
      metainfo,
      base::paste0("exextra[", tag_label,"]:")
    )
    newlines <- c(untouched, newmetainfo, "")
    base::writeLines(newlines, path, useBytes = TRUE)
  }
}

