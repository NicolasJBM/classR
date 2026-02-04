#' @name trees_edit_server
#' @title Edit course tree
#' @author Nicolas Mangin
#' @description Module allowing the user to change the hierarchical classification (tree) of documents and save these changes, either with the same name or a different one.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @param tree Character. Name of the tree.
#' @param jstree Reactive. Function containing a list of documents as a classification tree compatible with jsTreeR.
#' @param course_data Reactive. Function containing all the course data loaded with the course.
#' @param course_paths Reactive. Function containing a list of paths to the different folders and databases on local disk.
#' @return Save the tree tree as a tibble in the folder containing trees.
#' @importFrom dplyr anti_join
#' @importFrom dplyr bind_rows
#' @importFrom dplyr filter
#' @importFrom jsTreeR jstree
#' @importFrom jsTreeR jstreeDestroy
#' @importFrom jsTreeR renderJstree
#' @importFrom knitr knit2html
#' @importFrom shiny HTML
#' @importFrom shiny NS
#' @importFrom shiny actionButton
#' @importFrom shiny checkboxInput
#' @importFrom shiny h4
#' @importFrom shiny icon
#' @importFrom shiny modalButton
#' @importFrom shiny modalDialog
#' @importFrom shiny moduleServer
#' @importFrom shiny numericInput
#' @importFrom shiny observeEvent
#' @importFrom shiny reactive
#' @importFrom shiny removeModal
#' @importFrom shiny renderUI
#' @importFrom shiny req
#' @importFrom shiny selectInput
#' @importFrom shiny showModal
#' @importFrom shiny tagList
#' @importFrom shiny textInput
#' @importFrom shiny withMathJax
#' @importFrom shinyalert shinyalert
#' @importFrom shinybusy remove_modal_spinner
#' @importFrom shinybusy show_modal_spinner
#' @importFrom tibble tibble
#' @export

trees_edit_server <- function(id, tree, jstree, course_data, course_paths){
  ns <- shiny::NS(id)
  shiny::moduleServer(id, function(input, output, session) {

    documents <- NULL
    document_types <- NULL


    # Edit tree
    output$edittree <- jsTreeR::renderJstree({
      shiny::req(base::length(jstree()) >= 2)
      jsTreeR::jstreeDestroy(session, ns("edittree"))
      jsTreeR::jstree(
        nodes = jstree(),
        selectLeavesOnly = FALSE,
        checkboxes = TRUE,
        search = FALSE,
        searchtime = 250,
        dragAndDrop = TRUE,
        dnd = NULL,
        multiple = FALSE,
        types = NULL,
        sort = FALSE,
        unique = FALSE,
        wholerow = TRUE,
        contextMenu = TRUE,
        checkCallback = NULL,
        grid = base::list(
          columns = base::list(
            base::list(
              width = 650,
              header = "Title"
            ),
            base::list(
              width = 75,
              value = "type",
              header = "Type"
            ),
            base::list(
              width = 175,
              value = "file",
              header = "File"
            ),
            base::list(
              width = 100,
              value = "translations",
              header = "Translations"
            )
          ),
          width = 1000
        ),
        theme = "default"
      ) |>
        base::suppressMessages()
    })

    selected_document <- shiny::reactive({
      shiny::req(!base::is.null(input$edittree_selected))
      selection <- input$edittree_selected
      selection <- classR::trees_selected_json_to_tibble(selection)
      shiny::req("file" %in% base::names(selection))
      selection$file[1]
    })
    
    output$selecteddoc <- shiny::renderUI({
      shiny::req(!base::is.null(selected_document()))
      shiny::req(base::length(selected_document()) == 1)
      filepath <- base::paste0(
        course_paths()$subfolders$original, "/", selected_document()
      )
      shiny::req(base::file.exists(filepath))
      base::load(course_paths()$databases$propositions)
      test_parameters <- NA
      docformat <- "html"
      record_solution <- FALSE
      base::suppressWarnings(
        shiny::withMathJax(shiny::HTML(knitr::knit2html(
          text = base::readLines(filepath), quiet = TRUE, template = FALSE
        )))
      )
    })
    
    
    # Save tree
    shiny::observeEvent(input$savetree, {
      shinybusy::show_modal_spinner(
        spin = "orbit",
        text = "Please wait while the tree is saved..."
      )
      
      jstree <- input$edittree_full
      treename <- tree
      
      base::load(course_paths()$databases$documents)
      base::load(course_paths()$databases$doctypes)
      
      tbltree <- classR::jstree_to_tbltree(jstree, documents, document_types)
      base::save(tbltree, file = base::paste0(course_paths()$subfolders$tbltrees, "/", treename, ".RData"))
      
      jstree <- classR::tbltree_to_jstree(tbltree)
      base::save(jstree, file = base::paste0(course_paths()$subfolders$jstrees, "/", treename, ".RData"))
      
      shinybusy::remove_modal_spinner()
      shinyalert::shinyalert(
        "Saved!",
        base::paste0("Your tree has been saved as ", input$savetreeas,". Reload the course to see changes."),
        type = "success", closeOnEsc = FALSE, closeOnClickOutside = TRUE
      )
    })



    # Delete
    shiny::observeEvent(input$deletetree, {
      list_of_trees <- base::list.files(course_paths()$subfolders$jstrees)
      shiny::showModal(
        shiny::modalDialog(
          style = "background-color:#001F3F;color:#FFF;margin-top:300px;",
          shiny::selectInput(
            ns("treetodelete"),
            "Select the tree you wish to delete:",
            choices = c("", list_of_trees),
            selected = ""
          ),
          footer = shiny::tagList(
            shiny::modalButton("Cancel"),
            shiny::actionButton(
              ns("confirmdeletetree"), "OK",
              icon = shiny::icon("check"),
              style = "background-color:#007777;color:#FFF;"
            )
          )
        )
      )
    })

    shiny::observeEvent(input$confirmdeletetree, {
      shiny::removeModal()
      shiny::req(!base::is.null(input$treetodelete))
      if (input$treetodelete != ""){
        base::file.remove(
          base::paste0(
            course_paths()$subfolders$trees, "/", input$treetodelete
          )
        )
        base::file.remove(
          base::paste0(
            course_paths()$subfolders$jstrees, "/", input$treetodelete
          )
        )
        shinyalert::shinyalert(
          "Deleted!",
          "The selected tree has been deleted. Reload the course to enact changes.",
          type = "success",
          closeOnEsc = FALSE,
          closeOnClickOutside = TRUE
        )
      } else {
        shinyalert::shinyalert(
          "Canceled!",
          "No tree was selected for removal.",
          type = "warning",
          closeOnEsc = FALSE,
          closeOnClickOutside = TRUE
        )
      }
    })

  })
}

