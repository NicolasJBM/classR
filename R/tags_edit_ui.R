#' @name tags_edit_ui
#' @title Edit tags
#' @author Nicolas Mangin
#' @description Tabular form to change the attributes associated with tags or add new tags.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @return Modified tags.
#' @importFrom rhandsontable rHandsontableOutput
#' @importFrom shiny actionButton
#' @importFrom shiny column
#' @importFrom shiny fluidRow
#' @importFrom shiny icon
#' @importFrom shiny NS
#' @importFrom shiny selectInput
#' @importFrom shiny uiOutput
#' @export


tags_edit_ui <- function(id){
  ns <- shiny::NS(id)
  base::list(
    shiny::fluidRow(
      shiny::column(
        7,
        shiny::fluidRow(
          shiny::column(
            8,
            shiny::selectInput(
              ns("selecttag"),
              "Select a tag to edit:",
              choices = "", selected = "",
              width = "100%"
            )
          ),
          shiny::column(
            4,
            shiny::actionButton(
              ns("savetags"), "Save", icon = shiny::icon("floppy-disk"),
              style = "background-color:#006633;color:#FFF;width:100%;margin-top:25px;"
            )
          )
        ),
        rhandsontable::rHandsontableOutput(ns("display_custom_tags"))
      ),
      shiny::column(
        5,
        tags_search_icons_ui(ns("searchtags")),
        shiny::uiOutput(ns("edittags"))
      )
    )
  )
}

