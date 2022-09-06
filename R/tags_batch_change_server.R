#' @name tags_batch_change_server
#' @title Batch changes tags in documents
#' @author Nicolas Mangin
#' @description Interface allowing the user to apply changes to tags in all documents.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @param documents Tibble. List of documents
#' @param tags Tibble. List of tags, their values and attributes.
#' @import shiny
#' @import shinydashboardPlus
#' @importFrom shinyalert shinyalert
#' @importFrom shinybusy show_modal_spinner
#' @importFrom shinybusy remove_modal_spinner
#' @importFrom dplyr filter
#' @importFrom stringr str_detect
#' @export



tags_batch_change_server <- function(id, tags, documents){
  ns <- shiny::NS(id)
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      
      output$edittags <- shiny::renderUI({
        shiny::req(!base::is.null(tags))
        taglabels <- base::unique(base::as.character(tags$tag))
        shinydashboardPlus::box(
          width = 12, title = "Batch-edit tags in documents", solidHeader = TRUE,
          status = "danger", background = "navy",
          collapsible = TRUE, collapsed = TRUE,
          icon = shiny::icon("edit"),
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
                ns("deletetaglabel"), "Delete", icon = shiny::icon("trash-alt"),
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
      # 
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
      
      shiny::observe({
        shiny::req(!base::is.null(input$confirm_rm_taglabel))
        shiny::req(!base::is.null(tags))
        shiny::req(!base::is.null(documents))
        if (input$confirm_rm_taglabel){
          shinybusy::show_modal_spinner(
            spin = "orbit",
            text = base::paste0(
              'Please wait while ', input$slcttaglabel,' is removed from all documents...'
            )
          )
          classR::tags_remove(tags, documents, input$slcttaglabel)
          shinybusy::remove_modal_spinner()
          shinyalert::shinyalert(
            "Tag deleted!",
            "The tag has been removed from all documents and tables.",
            type = "success", closeOnEsc = FALSE,
            closeOnClickOutside = FALSE, inputId = "tagdeleted"
          )
        }
      })
      
      
      # Replace tag value ######################################################
      
      output$replacetagvalue <- shiny::renderUI({
        shiny::req(!base::is.null(input$slcttaglabel))
        values <- tags |>
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
              ns("replacetagvalue"), "Replace", icon = shiny::icon("edit"),
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
      
      shiny::observe({
        shiny::req(!base::is.null(input$confirm_replace_tagvalue))
        shiny::req(!base::is.null(documents))
        
        fromtagvalue <- shiny::isolate({input$fromtagvalue})
        totagvalue <- shiny::isolate({input$totagvalue})
        slcttaglabel <- shiny::isolate({input$slcttaglabel})
        
        if (input$confirm_replace_tagvalue){
          shinybusy::show_modal_spinner(
            spin = "orbit",
            text = base::paste0(
              '"Please wait while "', fromtagvalue,'" is replaced by "',
              totagvalue, '" in "', slcttaglabel,'" for all documents...'
            )
          )
          classR::tags_replace_value(
            tags, documents,
            slcttaglabel, fromtagvalue, totagvalue
          )
          shinybusy::remove_modal_spinner()
          shinyalert::shinyalert(
            "Value replaced!",
            "Reload the application to refresh documents' properties.",
            type = "success", closeOnEsc = FALSE,
            closeOnClickOutside = FALSE, inputId = ns("valuereplaced")
          )
        }
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
              ns("changetaglabel"), "Change", icon = shiny::icon("edit"),
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
      
      shiny::observe({
        shiny::req(!base::is.null(input$confirm_change_taglabel))
        shiny::req(!base::is.null(documents))
        
        slcttaglabel <- shiny::isolate({input$slcttaglabel})
        totaglabel <- shiny::isolate({input$totaglabel})
        
        if (input$confirm_change_taglabel){
          shinybusy::show_modal_spinner(
            spin = "orbit",
            text = base::paste0(
              'Please wait while the label "', slcttaglabel,
              '" is changed into "', totaglabel, '" in all documents...'
            )
          )
          classR::tags_change_label(
            tags, documents, slcttaglabel, totaglabel
          )
          shinybusy::remove_modal_spinner()
          shinyalert::shinyalert(
            "Label changed!",
            "The application will now reload to refresh documents' properties.",
            type = "success", closeOnEsc = FALSE,
            closeOnClickOutside = FALSE, inputId = ns("labelchanged")
          )
        }
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
        } else if (input$newtaglabel %in% tags$tag){
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
      
      shiny::observe({
        shiny::req(!base::is.null(input$confirm_add_taglabel))
        shiny::req(!base::is.null(documents))
        
        newtaglabel <- shiny::isolate({input$newtaglabel})
        
        if (input$confirm_add_taglabel){
          shinybusy::show_modal_spinner(
            spin = "orbit",
            text = base::paste0(
              'Please wait while "', newtaglabel,'" is added to all documents...'
            )
          )
          classR::tags_add(tags, documents, newtaglabel)
          shinybusy::remove_modal_spinner()
          shinyalert::shinyalert(
            "Tag added!", "The tag has been added to all documents.",
            type = "success", closeOnEsc = FALSE,
            closeOnClickOutside = FALSE, inputId = ns("tagadded")
          )
        }
      })
      
    }
  )}


