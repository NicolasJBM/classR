#' @name tags_search_icons_server
#' @title Search icons
#' @author Nicolas Mangin
#' @description Module allowing the user to search fontawesome icons to illustrate tags. The icon name can be added to the tags database through the tag edition module and appears with the document title.
#' @param id ID if the module to connect the user interface to the appropriate server side.
#' @importFrom shiny NS
#' @importFrom shiny icon
#' @importFrom shiny moduleServer
#' @importFrom shiny renderUI
#' @importFrom shiny req
#' @export


tags_search_icons_server <- function(id){
  ns <- shiny::NS(id)
  shiny::moduleServer(
    id,
    function(input, output, session) {
      output$display_icon <- shiny::renderUI({
        shiny::req(!base::is.null(input$slcticon))
        shiny::icon(
          input$slcticon,
          style = "width:100%;text-align:center;font-size:100px;"
        )
      })
    }
  )}
