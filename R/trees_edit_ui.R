#' @name trees_edit_ui
#' @title Edit course tree
#' @author Nicolas Mangin
#' @description Module allowing the user to change the hierarchical classification of documents and save these changes, either with the same name or a different one.
#' @param id ID if the module to connect the user interface to the appropriate server side.
#' @return Save the classification tree as a tibble in the folder "3_trees.
#' @importFrom jsTreeR jstreeOutput
#' @importFrom shiny actionButton
#' @importFrom shiny column
#' @importFrom shiny fluidRow
#' @importFrom shiny icon
#' @importFrom shiny NS
#' @importFrom shiny uiOutput
#' @export

trees_edit_ui <- function(id){
  ns <- shiny::NS(id)
  base::list(
    shiny::fluidRow(
      shiny::column(
        4,
        shiny::actionButton(
          ns("savecourse"), "Save course", icon = shiny::icon("floppy-disk"),
          style = "background-color:#006633;color:#FFF;
          width:100%;margin-top:25px;"
        ),
        shiny::uiOutput(ns("editcourse"))
      ),
      shiny::column(
        8,
        shiny::column(
          6,
          shiny::actionButton(
            ns("savetree"), "Save tree", icon = shiny::icon("floppy-disk"),
            style = "background-color:#006633;color:#FFF;
          width:100%;margin-top:25px;"
          )
        ),
        shiny::column(
          6,
          shiny::actionButton(
            ns("deletetree"), "Delete tree", icon = shiny::icon("trash"),
            style = "background-color:#990000;color:#FFF;
          width:100%;margin-top:25px;margin-bottom:25px;"
          )
        ),
        jsTreeR::jstreeOutput(ns("edittree"))
      )
    )
  )
}

