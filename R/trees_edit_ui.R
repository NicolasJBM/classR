#' @name trees_edit_ui
#' @title Edit course tree
#' @author Nicolas Mangin
#' @description Module allowing the user to change the hierarchical classification (tree) of documents and save these changes, either with the same name or a different one.
#' @param id ID if the module to connect the user interface to the appropriate server side.
#' @return Save the tree tree as a tibble in the folder containing trees.
#' @importFrom jsTreeR jstreeOutput
#' @importFrom shiny NS
#' @importFrom shiny actionButton
#' @importFrom shiny column
#' @importFrom shiny fluidRow
#' @importFrom shiny icon
#' @importFrom shiny uiOutput
#' @importFrom shinydashboardPlus box
#' @export



trees_edit_ui <- function(id){
  ns <- shiny::NS(id)
  base::list(
    shiny::fluidRow(
      shinydashboardPlus::box(
        title = "Definition",
        status = "navy",
        solidHeader = TRUE,
        width = 3,
        collapsible = TRUE,
        collapsed = FALSE,
        closable = FALSE,
        icon = shiny::icon("id-card"),
        shiny::actionButton(
          ns("savecourse"), "Save course", icon = shiny::icon("floppy-disk"),
          style = "background-color:#006600;color:#FFF;
          width:100%;margin-top:25px;"
        ),
        shiny::uiOutput(ns("editcourse"))
      ),
      shinydashboardPlus::box(
        title = "Classification",
        status = "primary",
        solidHeader = TRUE,
        width = 6,
        collapsible = TRUE,
        collapsed = FALSE,
        closable = FALSE,
        icon = shiny::icon("folder-tree"),
        shiny::fluidRow(
          shiny::column(
            6,
            shiny::actionButton(
              ns("savetree"), "Save tree", icon = shiny::icon("floppy-disk"),
              style = "background-color:#006600;color:#FFF;width:100%;margin-top:25px;"
            )
          ),
          shiny::column(
            6,
            shiny::actionButton(
              ns("deletetree"), "Delete tree", icon = shiny::icon("trash"),
              style = "background-color:#660000;color:#FFF;width:100%;margin-top:25px;margin-bottom:25px;"
            )
          )
        ),
        jsTreeR::jstreeOutput(ns("edittree"))
      ),
      shinydashboardPlus::box(
        title = "Display",
        status = "info",
        solidHeader = TRUE,
        width = 3,
        collapsible = TRUE,
        collapsed = FALSE,
        closable = FALSE,
        icon = shiny::icon("eye"),
        shiny::uiOutput(ns("selecteddoc"))
      )
    )
  )
}

