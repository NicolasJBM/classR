#' @name tags_search_icons_ui
#' @title Search icons
#' @author Nicolas Mangin
#' @description Module allowing the user to search fontawesome icons to illustrate tags.
#' @param id ID if the module to connect the user interface to the appropriate server side.
#' @import shiny
#' @importFrom shinydashboardPlus box
#' @importFrom fontawesome fa_metadata
#' @export

tags_search_icons_ui <- function(id){
  ns <- shiny::NS(id)
  shinydashboardPlus::box(
    title = "Choose icons and box colors",
    status = "purple",
    solidHeader = TRUE,
    width = 12,
    collapsible = TRUE,
    collapsed = FALSE,
    closable = FALSE,
    icon = shiny::icon("icons"),
    gradient = FALSE,
    shiny::fluidRow(
      shiny::column(
        6,
        base::suppressWarnings(shiny::selectInput(
          ns("slcticon"),
          "Select an icon name:",
          choices = fontawesome::fa_metadata()$icon_names,
          selected = "icons",
          width = "100%"
        )),
        shiny::uiOutput(ns("display_icon"))
      ),
      shiny::column(
        6,
        shiny::tags$p("navy", style = "text-align:center;color:#FFF;background-color:#001F3F;"),
        shiny::tags$p("primary", style = "text-align:center;color:#FFF;background-color:#3c8dbc;"),
        shiny::tags$p("info", style = "text-align:center;color:#FFF;background-color:#00c0ef;"),
        shiny::tags$p("teal", style = "text-align:center;color:#FFF;background-color:#39CCCC;"),
        shiny::tags$p("success", style = "text-align:center;color:#FFF;background-color:#00a65a;"),
        shiny::tags$p("warning", style = "text-align:center;color:#FFF;background-color:#f39c12;"),
        shiny::tags$p("orange", style = "text-align:center;color:#FFF;background-color:#ff851b;"),
        shiny::tags$p("danger", style = "text-align:center;color:#FFF;background-color:#f56954;"),
        shiny::tags$p("maroon", style = "text-align:center;color:#FFF;background-color:#D81B60;"),
        shiny::tags$p("purple", style = "text-align:center;color:#FFF;background-color:#605ca8;"),
        shiny::tags$p("black", style = "text-align:center;color:#FFF;background-color:#111111;")
      )
    )
  )
}
