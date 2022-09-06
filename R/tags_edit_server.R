#' @name tags_edit_server
#' @title Edit tags
#' @author Nicolas Mangin
#' @description Tabular form to change the attributes associated with tags or add new tags.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @param documents Tibble. List of documents
#' @param tags Tibble. List of tags, their values and attributes.
#' @return Modified tags.
#' @import shiny
#' @importFrom rhandsontable renderRHandsontable
#' @importFrom dplyr mutate
#' @importFrom rhandsontable rhandsontable
#' @importFrom fontawesome fa_metadata
#' @importFrom rhandsontable hot_col
#' @importFrom rhandsontable hot_cols
#' @importFrom rhandsontable hot_context_menu
#' @importFrom rhandsontable hot_to_r
#' @importFrom dplyr arrange
#' @importFrom dplyr select
#' @importFrom shinyalert shinyalert
#' @export


tags_edit_server <- function(id, tags, documents){
  ns <- shiny::NS(id)
  shiny::moduleServer(id, function(input, output, session) {
    
    boxcolor <- NULL
    value <- NULL
    count <- NULL
    
    output$display_custom_tags <- rhandsontable::renderRHandsontable({
      tags |>
        dplyr::mutate(
          icon = base::factor(
            icon,
            levels = fontawesome::fa_metadata()$icon_names
          ),
          boxcolor = base::factor(boxcolor, levels = c(
            "navy","primary","info","teal","success",
            "warning","orange","danger","maroon",
            "purple","black"
          ))
        ) |>
        rhandsontable::rhandsontable(
          height = 500, width = "100%", rowHeaders = NULL, stretchH = "all"
        ) |>
        rhandsontable::hot_col(4, readOnly = TRUE) |>
        rhandsontable::hot_cols(
          colWidths = c("20%","10%","20%","10%","20%","20%")
        ) |>
        rhandsontable::hot_context_menu(
          allowRowEdit = TRUE, allowColEdit = FALSE
        )
    })
    
    new_tags <- shiny::reactive({
      shiny::req(!base::is.null(input$display_custom_tags))
      tags <- rhandsontable::hot_to_r(input$display_custom_tags) |>
        dplyr::arrange(tag, order) |>
        dplyr::select(tag, order, value, count, icon, boxcolor)
    })
    
    shiny::observeEvent(input$savetags, {
      tags <- new_tags() |>
        dplyr::arrange(tag, order)
      base::save(tags, file = "2_documents/tags.RData")
      shinyalert::shinyalert(
        "Tags saved!",
        'The changes you applied to tags have been saved.',
        type = "success", closeOnEsc = FALSE, closeOnClickOutside = TRUE
      )
    })
    
    return(new_tags)
  })
}

