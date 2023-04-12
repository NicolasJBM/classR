#' @name trees_edit_server
#' @title Edit course tree
#' @author Nicolas Mangin
#' @description Module allowing the user to change the hierarchical tree of documents and save these changes, either with the same name or a different one.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @param tree Reactive. Function containing a list of documents as a classification tree compatible with jsTreeR.
#' @param course_data Reactive. Function containing all the course data loaded with the course.
#' @param course_paths Reactive. Function containing a list of paths to the different folders and databases on local disk.
#' @return Save the tree tree as a tibble in the folder "3_trees".
#' @importFrom dplyr anti_join
#' @importFrom dplyr bind_rows
#' @importFrom dplyr filter
#' @importFrom jsTreeR jstree
#' @importFrom jsTreeR jstreeDestroy
#' @importFrom jsTreeR renderJstree
#' @importFrom shiny actionButton
#' @importFrom shiny h4
#' @importFrom shiny icon
#' @importFrom shiny modalButton
#' @importFrom shiny modalDialog
#' @importFrom shiny moduleServer
#' @importFrom shiny NS
#' @importFrom shiny numericInput
#' @importFrom shiny observeEvent
#' @importFrom shiny removeModal
#' @importFrom shiny renderUI
#' @importFrom shiny req
#' @importFrom shiny selectInput
#' @importFrom shiny showModal
#' @importFrom shiny tagList
#' @importFrom shiny textInput
#' @importFrom shinyalert shinyalert
#' @importFrom shinybusy remove_modal_spinner
#' @importFrom shinybusy show_modal_spinner
#' @importFrom shiny withMathJax
#' @importFrom shiny HTML
#' @importFrom knitr knit2html
#' @importFrom tibble tibble
#' @export

trees_edit_server <- function(id, tree, course_data, course_paths){
  ns <- shiny::NS(id)
  shiny::moduleServer(id, function(input, output, session) {

    output$editcourse <- shiny::renderUI({
      shiny::req(!base::is.na(tree()$course))
      base::list(
        shiny::h4(tree()$course$tree[1]),
        shiny::textInput(
          ns("course"), "Course:", value = tree()$course$course[1], width = "100%"
        ),
        shiny::textInput(
          ns("authors"), "Authors:",
          value = tree()$course$authors[1], width = "100%"
        ),
        shiny::textInput(
          ns("institution"), "Institution:",
          value = tree()$course$institution[1], width = "100%"
        ),
        shiny::textInput(
          ns("program"), "Program:",
          value = tree()$course$program[1], width = "100%"
        ),
        shiny::textInput(
          ns("programlevel"), "Program level:",
          value = tree()$course$program_level[1], width = "100%"
        ),
        shiny::textInput(
          ns("group"), "Group:",
          value = tree()$course$group[1], width = "100%"
        ),
        shiny::numericInput(
          ns("year"), "Year:",
          value = tree()$course$year[1], width = "100%"
        ),
        shiny::textInput(
          ns("website"), "Website:",
          value = tree()$course$website[1], width = "100%"
        )
      )
    })


    # Save course
    shiny::observeEvent(input$savecourse, {
      other_courses <- course_data()$courses |>
        dplyr::anti_join(tree()$course, by = "tree")
      edtided <- tibble::tibble(
        tree = base::as.character(tree()$course$tree[1]),
        course = base::as.character(input$course),
        authors = base::as.character(input$authors),
        institution = base::as.character(input$institution),
        program = base::as.character(input$program),
        program_level = base::as.character(input$programlevel),
        group = base::as.character(input$group),
        year =  base::as.character(input$year),
        website = base::as.character(input$website)
      )
      courses <- dplyr::bind_rows(edtided, other_courses)
      base::save(courses, file = course_paths()$databases$courses)
      shinyalert::shinyalert(
        "Saved!",
        base::paste0("Changes to the course information have been saved. Reload the course to see changes."),
        type = "success", closeOnEsc = FALSE, closeOnClickOutside = TRUE
      )
    })


    # Edit tree
    output$edittree <- jsTreeR::renderJstree({
      shiny::req(!base::is.na(tree()$jstree))
      jsTreeR::jstreeDestroy(session, ns("edittree"))
      jsTreeR::jstree(
        nodes = tree()$jstree,
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
        grid = NULL,
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
      treename <- tree()$course$tree[1]
      base::save(
        jstree,
        file = base::paste0(course_paths()$subfolders$jstrees, "/", treename)
      )
      tree <- classR::trees_json_to_tibble(jstree, maxlevel = 10, course_data())
      base::save(
        tree,
        file = base::paste0(course_paths()$subfolders$trees, "/", treename)
      )
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
        courses <- course_data()$courses |>
          dplyr::filter(tree != input$treetodelete)
        base::save(courses, file = course_paths()$databases$courses)
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

