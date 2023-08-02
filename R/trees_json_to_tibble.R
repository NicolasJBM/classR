#' @name trees_json_to_tibble
#' @title Transform a JSON list into a tibble
#' @author Nicolas Mangin
#' @description Function to transforms a document hierarchical classification into a tibble and update all document-related information
#' @param json_file List. Document list as a classification tree.
#' @param maxlevel Maximum number of nested levels to be expected. Set to a high number to give leeway
#' @param course_data Reactive. Function containing all the course data loaded with the course.
#' @return Tibble. List of documents with positions extracted from the classification tree and additional variables gathered from the document list.
#' @importFrom dplyr bind_rows
#' @importFrom dplyr case_when
#' @importFrom dplyr filter
#' @importFrom dplyr full_join
#' @importFrom dplyr group_by
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_all
#' @importFrom dplyr select
#' @importFrom dplyr starts_with
#' @importFrom dplyr ungroup
#' @importFrom purrr map
#' @importFrom purrr map_chr
#' @importFrom stringr str_remove_all
#' @importFrom tibble enframe
#' @importFrom tidyr nest
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr pivot_wider
#' @importFrom tidyr separate
#' @importFrom tidyr unnest
#' @export


trees_json_to_tibble <- function(json_file, maxlevel = 10, course_data){
  
  boxcolor <- NULL
  data <- NULL
  docnbr <- NULL
  info <- NULL
  level <- NULL
  name <- NULL
  position <- NULL
  text <- NULL
  title <- NULL
  type <- NULL
  
  documents <- course_data$documents
  document_types <- course_data$document_types
  
  # This condition handles the initial case of nothing being classified.
  if (base::length(json_file[[1]]$children) > 0 |
      base::length(json_file[[2]]$children) > 0){
    
    # Separate the levels of the list base on dot (.) separating information and
    # remove the extra columns without information. Note that because of this,
    # information should not contain dots but rather underscores (_)
    levels <- base::paste0("LEV", 1:maxlevel)
    table_file <- tibble::enframe(base::unlist(json_file)) |>
      tidyr::separate(name, into = levels, sep = "\\.") |>
      base::suppressWarnings()
    table_file <- table_file[, base::apply(table_file, 2, function(x)
      base::sum(base::is.na(x)/base::length(x))) < 1]
    
    # Remove empty information about the prior structure of the list and
    # gather levels in a single variable, removing empty rows.
    table_file <- table_file |>
      dplyr::mutate_all(
        function(x) base::replace(x, (x == "children" | x == "data"),  NA)
      ) |> 
      tidyr::pivot_longer(
        cols = dplyr::starts_with("LEV"),
        names_to = "level",
        values_to = "info"
      ) |>
      dplyr::filter(
        info %in% c("text","title","file","code","language","modified","type","document","icon")
      ) |>
      dplyr::mutate(
        level = base::as.numeric(stringr::str_remove_all(level, "LEV")),
        docnbr = 0
      )
    
    # Identify documents (new document/folder every time the info "text" is met)
    # Note that this assumes that the name of each entry is stored in a variable
    # called "text" (to follow jsTreeR convention)
    doc <- 0
    for (row in base::seq_len(base::nrow(table_file))){
      if (table_file[row,"info"] == "text") doc <- doc+1
      table_file[row,"docnbr"] <- doc
    }
    
    # Spread the table for each document so that information describing them
    # becomes variables and then remove variables which will be updated with the
    # table "documents". Folders and document still within documents are kept.
    # However, documents which are not in "documents" are removed from the list.
    table_file <- table_file |>
      dplyr::group_by(docnbr) |>
      dplyr::mutate(level = base::min(level)) |>
      tidyr::nest() |>
      dplyr::mutate(data = purrr::map(
        data,
        function(x) tidyr::pivot_wider(
          x, id_cols = , names_from = "info", values_from = "value"
        )
      )) |>
      tidyr::unnest(data) |>
      dplyr::select(docnbr, level, text, file) |>
      dplyr::mutate(position = "") |>
      dplyr::filter(base::is.na(file)|(file %in% documents$file))
    
    # Create a vector "coordinates" which will be incremented at the level of
    # each document. The position of each document is obtained by collapsing
    # this vector.
    coordinates <- base::rep(0, base::max(table_file$level))
    for (row in base::seq_len(base::nrow(table_file))){
      tmplev <- base::unlist(table_file[row,"level"])
      coordinates[tmplev] <- coordinates[tmplev]+1
      # The next condition is necessary to avoid adding a phantom level
      # compromising the final list
      if (base::length(coordinates) >= (tmplev+1)){
        coordinates[(tmplev+1):base::length(coordinates)] <- 0
      }
      table_file[row,"position"] <- base::paste(coordinates, collapse = ".")
    }
    
    # Add information from "documents" and "types" which have been updated
    # in a prior step.
    types <- dplyr::select(document_types, type, icon, boxcolor)
    table_file <- table_file |>
      dplyr::ungroup(docnbr) |>
      dplyr::select(position, text, file) |>
      dplyr::full_join(documents, by = "file") |>
      dplyr::left_join(types, by = "type") |>
      dplyr::filter(!base::is.na(position)) |>
      dplyr::mutate(
        text = dplyr::case_when(
          base::is.na(file) ~ text,
          TRUE ~ title
        ),
        icon = dplyr::case_when(
          base::is.na(file) ~ "folder-open",
          TRUE ~ icon
        ),
        boxcolor = dplyr::case_when(
          base::is.na(file) ~ "black",
          TRUE ~ icon
        )
      )
    
    # List documents in "documents" which are not listed in the hierarchy and
    # add them as "unclassified". Note that this procedure assumes that 
    # children in unclassified have been removed prior to applying this function
    not_classified <- documents |>
      dplyr::left_join(
        dplyr::select(types, type, icon, boxcolor), by = "type"
      ) |>
      dplyr::filter(!(file %in% table_file$file))
    if (base::nrow(not_classified) > 0){
      not_classified <- not_classified |>
        dplyr::mutate(
          position = base::seq_len(base::nrow(not_classified)),
          text = title
        ) |>
        dplyr::mutate(position = purrr::map_chr(
          position,
          function(x) base::paste0(
            c("3",x,(base::rep(0, base::length(coordinates)-2))),
            collapse = "."
          )
        ))
      table_file <- dplyr::bind_rows(table_file, not_classified)
    }
    
  } else {
    coordinates <- base::rep(0, 2) 
    table_file <- documents |>
      dplyr::left_join(
        dplyr::select(types, type, icon, boxcolor), by = "type"
      )
    table_file <- table_file |>
      dplyr::mutate(
        position = base::seq_len(base::nrow(table_file)),
        text = title
      ) |>
      dplyr::mutate(position = purrr::map_chr(
        position,
        function(x) base::paste0(
          c("3",x,(base::rep(0, base::length(coordinates)-2))),
          collapse = "."
        )
      ))
  }
  
  return(table_file)
}
