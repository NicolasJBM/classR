#' @name tags_edit_server
#' @title Edit tags
#' @author Nicolas Mangin
#' @description Tabular form to change the attributes associated with tags or add new tags.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @param course_data Reactive. Function containing all the course data loaded with the course.
#' @param course_paths Reactive. Function containing a list of paths to the different folders and databases on local disk.
#' @return Modified tags.
#' @import shiny
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom fontawesome fa_metadata
#' @importFrom rhandsontable hot_col
#' @importFrom rhandsontable hot_cols
#' @importFrom rhandsontable hot_context_menu
#' @importFrom rhandsontable hot_to_r
#' @importFrom rhandsontable renderRHandsontable
#' @importFrom rhandsontable rhandsontable
#' @importFrom shiny actionButton
#' @importFrom shiny column
#' @importFrom shiny fluidRow
#' @importFrom shiny icon
#' @importFrom shiny isolate
#' @importFrom shiny moduleServer
#' @importFrom shiny NS
#' @importFrom shiny observe
#' @importFrom shiny observeEvent
#' @importFrom shiny reactive
#' @importFrom shiny renderUI
#' @importFrom shiny req
#' @importFrom shiny selectInput
#' @importFrom shiny textInput
#' @importFrom shiny uiOutput
#' @importFrom shiny updateSelectInput
#' @importFrom shinyalert shinyalert
#' @importFrom shinybusy remove_modal_spinner
#' @importFrom shinybusy show_modal_spinner
#' @importFrom shinydashboardPlus box
#' @importFrom stringr str_detect
#' @export


tags_edit_server <- function(id, course_data, course_paths){
  ns <- shiny::NS(id)
  shiny::moduleServer(id, function(input, output, session) {
    
    boxcolor <- NULL
    value <- NULL
    count <- NULL
    
    shiny::observe({
      shiny::req(base::length(course_data()$tags) > 1)
      tags <- c("", base::as.character(base::unique(course_data()$tags$tag)))
      shiny::updateSelectInput(
        session, "selecttag", choices = tags
      )
    })
    
    output$display_custom_tags <- rhandsontable::renderRHandsontable({
      shiny::req(base::length(course_data()$tags) > 1)
      if (input$selecttag != ""){
        course_data()$tags |>
          dplyr::filter(tag == input$selecttag) |>
          dplyr::mutate(
            tag = base::factor(
              tag,
              levels = base::unique(course_data()$tags$tag)
            ),
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
      }
    })
    
    shiny::observeEvent(input$savetags, {
      shiny::req(!base::is.null(input$display_custom_tags))
      unchanged <- course_data()$tags |>
        dplyr::filter(tag != input$selecttag)
      edited <- rhandsontable::hot_to_r(input$display_custom_tags) |>
        dplyr::mutate(tag = base::as.character(tag))
      tags <- dplyr::bind_rows(edited, unchanged) |>
        dplyr::arrange(tag, order)
      base::save(tags, file = course_paths()$databases$tags)
      shinyalert::shinyalert(
        "Tags saved!",
        'Reload the course to implement your changes.',
        type = "success", closeOnEsc = FALSE, closeOnClickOutside = TRUE
      )
    })
    
    
    
    tags_search_icons_server("searchtags")
    
    
    
    
    ############################################################################
    # Change tags ##############################################################
    ############################################################################
    
    output$edittags <- shiny::renderUI({
      shiny::req(base::length(course_data()$tags) > 1)
      taglabels <- base::unique(base::as.character(course_data()$tags$tag))
      shinydashboardPlus::box(
        width = 12, title = "Batch-edit tags in documents", solidHeader = TRUE,
        status = "danger", background = "navy",
        collapsible = TRUE, collapsed = FALSE,
        icon = shiny::icon("pen-to-square"),
        shiny::fluidRow(
          shiny::column(
            8,
            shiny::selectInput(
              ns("slcttaglabel"), "Select a tag to edit:",
              choices = c("", taglabels), selected = ""
            )
          ),
          shiny::column(
            4,
            shiny::actionButton(
              ns("deletetaglabel"), "Delete", icon = shiny::icon("trash-can"),
              style = "background-color:#990000;color:#FFF;width:100%;margin-top:25px;"
            )
          )
        ),
        shiny::uiOutput(ns("changetaglabel")),
        shiny::uiOutput(ns("replacetagvalue")),
        shiny::uiOutput(ns("addtaglabel"))
      )
    })
    
    
    
    # Delete tags ############################################################
    
    shiny::observeEvent(input$deletetaglabel, {
      shinyalert::shinyalert(
        title = "Tag removal?",
        text = base::paste0(
          "Are you sure you want to remove all instances of ",
          input$slcttaglabel, " in all your documents?"
        ),
        type = "warning", closeOnEsc = FALSE, closeOnClickOutside = FALSE,
        inputId = "confirm_rm_taglabel",
        showCancelButton = TRUE, showConfirmButton = TRUE,
        confirmButtonText = "Yes", confirmButtonCol = "#009900",
        cancelButtonText = "No"
      )
    })
    
    shiny::observeEvent(input$confirm_rm_taglabel, {
      
      docs <- shiny::isolate({ course_data()$documents })
      tags <- shiny::isolate({ course_data()$tags })
      paths <- shiny::isolate({ course_paths })
      tagtoremove <- shiny::isolate({ input$slcttaglabel })
      
      shiny::req(tagtoremove %in% base::names(docs))
      shiny::req(tagtoremove %in% tags$tag)
      
      shinybusy::show_modal_spinner(
        spin = "orbit",
        text = base::paste0(
          'Please wait while ', tagtoremove,' is removed from all documents...'
        )
      )
      classR::tags_remove(paths, tagtoremove)
      shinybusy::remove_modal_spinner()
      shinyalert::shinyalert(
        "Tag deleted!",
        "The tag has been removed from all documents and tables. Refresh documents and tags and reload the course to see changes.",
        type = "success", closeOnEsc = FALSE,
        closeOnClickOutside = FALSE, inputId = "tagdeleted"
      )
    })
    
    
    
    # Change tag label #######################################################
    
    output$changetaglabel <- shiny::renderUI({
      shiny::fluidRow(
        shiny::column(
          8,
          shiny::textInput(
            ns("totaglabel"), "Change the tag label to", value = "tag_"
          )
        ),
        shiny::column(
          4,
          shiny::actionButton(
            ns("changetaglabel"), "Change", icon = shiny::icon("pen-to-square"),
            style = "background-color:#000099;color:#FFF;width:100%;margin-top:25px;"
          )
        )
      )
    })
    
    shiny::observeEvent(input$changetaglabel, {
      if (input$slcttaglabel == ""){
        shinyalert::shinyalert(
          "No tag selected!",
          "Select first the tag the label of which should be changed.",
          type = "warning", closeOnEsc = FALSE, closeOnClickOutside = TRUE
        )
      } else if (input$totaglabel == "tag_" |
                 !stringr::str_detect(input$totaglabel, "^tag_")){
        shinyalert::shinyalert(
          "Inadequate label for the tag!",
          'The new tag label must start with "tag_" and have a name.',
          type = "error", closeOnEsc = FALSE, closeOnClickOutside = TRUE
        )
      } else if (input$totaglabel %in% tags$tag){
        shinyalert::shinyalert(
          "Tag already existing!",
          "This tag label is already used in your documents.",
          type = "warning", closeOnEsc = FALSE, closeOnClickOutside = TRUE
        )
      } else {
        shinyalert::shinyalert(
          title = "Label replacement?",
          text = base::paste0(
            'Are you sure you want to replace the label "',
            input$slcttaglabel, '" by the label "', input$totaglabel,
            '" in all your documents?'
          ),
          type = "warning", closeOnEsc = FALSE, closeOnClickOutside = FALSE,
          inputId = "confirm_change_taglabel",
          showCancelButton = TRUE, showConfirmButton = TRUE,
          confirmButtonText = "Yes", confirmButtonCol = "#009900",
          cancelButtonText = "No"
        )
      }
    })
    
    shiny::observeEvent(input$confirm_change_taglabel, {
      
      docs <- shiny::isolate({ course_data()$documents })
      tags <- shiny::isolate({ course_data()$tags })
      paths <- shiny::isolate({ course_paths })
      slcttaglabel <- shiny::isolate({input$slcttaglabel})
      totaglabel <- shiny::isolate({input$totaglabel})
      
      shiny::req(base::length(tags) > 0)
      shiny::req(slcttaglabel != "tag_")
      shiny::req(slcttaglabel %in% base::names(docs))
      shiny::req(slcttaglabel %in% tags$tag)
      shiny::req(totaglabel != "tag_")
      shiny::req(!(totaglabel %in% base::names(docs)))
      shiny::req(!(totaglabel %in% tags$tag))
      
      shinybusy::show_modal_spinner(
        spin = "orbit",
        text = base::paste0(
          'Please wait while the label "', slcttaglabel,
          '" is changed into "', totaglabel, '" in all documents...'
        )
      )
      classR::tags_change_label(
        paths, slcttaglabel, totaglabel
      )
      shinybusy::remove_modal_spinner()
      shinyalert::shinyalert(
        "Label changed!",
        "Refresh documents and tags and reload the course to see changes.",
        type = "success", closeOnEsc = FALSE,
        closeOnClickOutside = FALSE, inputId = ns("labelchanged")
      )
    })
    
    
    
    # Replace tag value ######################################################
    
    output$replacetagvalue <- shiny::renderUI({
      shiny::req(!base::is.null(input$slcttaglabel))
      shiny::req(base::length(course_data()$tags) > 1)
      values <- course_data()$tags |>
        dplyr::filter(tag == input$slcttaglabel)
      shiny::fluidRow(
        shiny::column(
          4,
          shiny::selectInput(
            ns("fromtagvalue"), "Replace the value",
            choices = c("", values$value),
            selected = ""
          )
        ),
        shiny::column(
          4,
          shiny::selectInput(
            ns("totagvalue"), "by the value",
            choices = c("", values$value),
            selected = ""
          )
        ),
        shiny::column(
          4,
          shiny::actionButton(
            ns("replacetagvalue"), "Replace", icon = shiny::icon("right-left"),
            style = "background-color:#000099;color:#FFF;width:100%;margin-top:25px;"
          )
        )
      )
    })
    
    shiny::observeEvent(input$replacetagvalue, {
      if (input$fromtagvalue == "" | input$totagvalue == ""){
        shinyalert::shinyalert(
          "No tag or value selected!",
          "Please select a tag, replaced value, and replacement value.",
          type = "error", closeOnEsc = FALSE, closeOnClickOutside = TRUE
        )
      } else {
        shinyalert::shinyalert(
          title = "Value replacement?",
          text = base::paste0(
            'Are you sure you want to replace "',
            input$fromtagvalue, '" by "', input$totagvalue,
            '" for the tag "', input$slcttaglabel,
            '" in all your documents?'
          ),
          type = "warning", closeOnEsc = FALSE, closeOnClickOutside = FALSE,
          inputId = "confirm_replace_tagvalue",
          showCancelButton = TRUE, showConfirmButton = TRUE,
          confirmButtonText = "Yes", confirmButtonCol = "#009900",
          cancelButtonText = "No"
        )
      }
    })
    
    shiny::observeEvent(input$confirm_replace_tagvalue, {
      
      docs <- shiny::isolate({ course_data()$documents })
      tags <- shiny::isolate({ course_data()$tags })
      paths <- shiny::isolate({ course_paths })
      slcttaglabel <- shiny::isolate({input$slcttaglabel})
      fromtagvalue <- shiny::isolate({input$fromtagvalue})
      totagvalue <- shiny::isolate({input$totagvalue})
      
      shiny::req(base::length(tags) > 0)
      shiny::req(slcttaglabel != "tag_")
      shiny::req(slcttaglabel %in% base::names(docs))
      shiny::req(fromtagvalue %in% course_data()$tags$value)
      shiny::req(totagvalue %in% course_data()$tags$value)
      
      shinybusy::show_modal_spinner(
        spin = "orbit",
        text = base::paste0(
          '"Please wait while "', fromtagvalue,'" is replaced by "',
          totagvalue, '" in "', slcttaglabel,'" for all documents...'
        )
      )
      classR::tags_replace_value(
        paths, slcttaglabel, fromtagvalue, totagvalue
      )
      shinybusy::remove_modal_spinner()
      shinyalert::shinyalert(
        "Value replaced!",
        "Refresh documents and tags and reload the course to see changes.",
        type = "success", closeOnEsc = FALSE,
        closeOnClickOutside = FALSE, inputId = ns("valuereplaced")
      )
    })
    
    
    
    # Add a tag ##############################################################
    
    output$addtaglabel <- shiny::renderUI({
      shiny::fluidRow(
        shiny::column(
          8,
          shiny::textInput(
            ns("newtaglabel"), "Add a new tag called:", value = "tag_"
          )
        ),
        shiny::column(
          4,
          shiny::actionButton(
            ns("addtaglabel"), "Add", icon = shiny::icon("plus"),
            style = "background-color:#009900;color:#FFF;width:100%;margin-top:25px;"
          )
        )
      )
    })
    
    shiny::observeEvent(input$addtaglabel, {
      if (input$newtaglabel == "tag_" |
          !stringr::str_detect(input$newtaglabel, "^tag_")){
        shinyalert::shinyalert(
          "Inadequate label for the tag!",
          'The tag label must start with "tag_" and have a name.',
          type = "error", closeOnEsc = FALSE, closeOnClickOutside = TRUE
        )
      } else if (input$newtaglabel %in% course_data()$tags$tag){
        shinyalert::shinyalert(
          "Tag already existing!",
          "This tag label is already used in your documents.",
          type = "warning", closeOnEsc = FALSE, closeOnClickOutside = TRUE
        )
      } else {
        shinyalert::shinyalert(
          title = "Tag addition?",
          text = base::paste0(
            "Are you sure you want to add ",
            input$newtaglabel, " in all your documents?"
          ),
          type = "warning", closeOnEsc = FALSE, closeOnClickOutside = FALSE,
          inputId = "confirm_add_taglabel",
          showCancelButton = TRUE, showConfirmButton = TRUE,
          confirmButtonText = "Yes", confirmButtonCol = "#009900",
          cancelButtonText = "No"
        )
      }
    })
    
    shiny::observeEvent(input$confirm_add_taglabel, {
      
      docs <- shiny::isolate({ course_data()$documents })
      tags <- shiny::isolate({ course_data()$tags })
      paths <- shiny::isolate({ course_paths })
      newtaglabel <- shiny::isolate({input$newtaglabel})
      
      shiny::req(!(newtaglabel %in% base::names(docs)))
      shiny::req(!(newtaglabel %in% tags$tag))
      shiny::req(base::nchar(newtaglabel)>0)
      shiny::req(newtaglabel != "tag_")
      
      shinybusy::show_modal_spinner(
        spin = "orbit",
        text = base::paste0(
          'Please wait while "', newtaglabel,'" is added to all documents...'
        )
      )
      classR::tags_add(paths, newtaglabel)
      shinybusy::remove_modal_spinner()
      shinyalert::shinyalert(
        "Tag added!", "The tag has been added to all documents. Refresh documents and tags and reload the course to see changes.",
        type = "success", closeOnEsc = FALSE,
        closeOnClickOutside = FALSE, inputId = ns("tagadded")
      )
    })
    
  })
}

