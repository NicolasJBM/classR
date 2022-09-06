#' @name tags_edit_ui
#' @title Edit tags
#' @author Nicolas Mangin
#' @description Tabular form to change the attributes associated with tags or add new tags.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @return Modified tags.
#' @import shiny
#' @importFrom rhandsontable rHandsontableOutput
#' @export


tags_edit_ui <- function(id){
  ns <- shiny::NS(id)
  base::list(
    shiny::actionButton(
      ns("savetags"), "Save", icon = shiny::icon("save"),
      style = "background-color:#006633;color:#FFF;width:100%;margin-top:10px;margin-bottom:10px;"
    ),
    rhandsontable::rHandsontableOutput(ns("display_custom_tags"))
  )
}

