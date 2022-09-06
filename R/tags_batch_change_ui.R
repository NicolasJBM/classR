#' @name tags_batch_change_ui
#' @title Batch changes tags in documents
#' @author Nicolas Mangin
#' @description Interface allowing the user to apply changes to tags in all documents.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @return Interface facilitating the modification of tag labels or values in all documents.
#' @import shiny
#' @export


tags_batch_change_ui <- function(id){
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("edittags"))
}

