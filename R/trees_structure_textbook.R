#' @name trees_structure_textbook
#' @title Create a textbook structure
#' @author Nicolas Mangin
#' @description Function creating the textbook structure associated with a tree.
#' @param tree list.
#' @param tree_name Character. Name of the tree.
#' @param website Character. Address of the website where the textbook is hosted.
#' @return Tibble with all necessary information about the pages of the textbook.
#' @importFrom stringr str_count
#' @importFrom dplyr filter
#' @importFrom stringr str_detect
#' @importFrom dplyr arrange
#' @importFrom dplyr mutate
#' @importFrom tidyr separate
#' @importFrom dplyr mutate_at
#' @importFrom dplyr vars
#' @importFrom dplyr starts_with
#' @importFrom stringr str_replace_all
#' @importFrom stats na.omit
#' @importFrom tidyr unite
#' @importFrom dplyr bind_cols
#' @importFrom dplyr select
#' @export


trees_structure_textbook <- function(tree, tree_name = "", website = ""){

  position <- NULL
  type <- NULL
  LEVEL_0 <- NULL
  folder <- NULL
  language <- NULL
  title <- NULL
  document <- NULL
  section <- NULL
  prefix <- NULL
  complement <- NULL
  link <- NULL
  languageweb <- NULL
  code <- NULL


  tree_name <- stringr::str_remove(tree_name, ".RData$")
  website <- stringr::str_remove(website, "/$")

  levnbr <- stringr::str_count(tree$position[1], "\\.")

  files <- tree |>
    dplyr::filter(
      stringr::str_detect(position, "^1."),
      type == "Page",
      !base::is.na(file)
    )
  
  if (base::nrow(files) > 0){
    
    files$position <- stringr::str_remove_all(files$position, "^1.")
    
    positions <- files |>
      dplyr::select(position) |>
      tidyr::separate(
        position, into = base::paste0("LEV", base::seq_len(levnbr)), sep = "\\."
      ) |>
      dplyr::mutate_all(base::as.numeric) |>
      dplyr::mutate_all(function(x) x-1) |>
      dplyr::mutate_all(function(x) base::replace(x, x<=0, 0))
    
    if (base::nrow(positions) > 1){
      coordinates <- base::list()
      coordinates[[1]] <- base::rep(0, levnbr)
      for (i in 2:base::nrow(positions)){
        tmp1 <- coordinates[[i-1]]
        tmp2 <- base::as.numeric((positions[i,]-positions[i-1,]))
        tmp2 <- base::replace(tmp2, tmp2>1, 1)
        tmp3 <- tmp1 + tmp2
        coordinates[[i]] <- base::replace(tmp3, tmp3<0, 0)
      }
      coordinates <- base::unlist(base::lapply(
        coordinates, function(x) base::paste(x, collapse = ".")
      ))
      
      files <- files |>
        dplyr::mutate(position = coordinates) |>
        tidyr::separate(position, into = base::paste0(
          "LEVEL_",1:(levnbr)), sep = "\\."
        ) |>
        dplyr::mutate(folder = "")
    } else {
      files <- files |>
        dplyr::mutate(
          LEVEL_1 = "1",
          folder = ""
        ) |>
        dplyr::select(-position)
    }
    
    positions <- files |>
      dplyr::select(dplyr::starts_with("LEVEL_")) |>
      dplyr::mutate_all(
        function(x) base::as.character(base::replace(x, x == 0, NA))
      ) |>
      base::as.data.frame()
    
    folders <- positions
    for (row in base::seq_len(base::nrow(folders))){
      for (col in base::seq_len(base::length(folders))){
        tmptest <- stats::na.omit(base::as.character(positions[row,col]))
        tmpid <- stats::na.omit(base::as.character(positions[row,1:col]))
        if (base::length(tmptest) > 0 & base::length(tmpid) > 0){
          folders[row,col] <- base::paste(c(
            "children", tmpid
          ), collapse = "-")
        } else folders[row,col] <- NA
      }
    }
    
    section_nbr <- positions |>
      tidyr::unite("section", base::names(positions), na.rm = TRUE, sep = "")
    
    folders <- tidyr::unite(
      folders, "folder", base::names(folders), sep = "/", na.rm = TRUE
    ) |>
      dplyr::mutate(folder = base::paste(tree_name, folder, sep = "/"))
    
    textbook_structure <- files |>
      dplyr::bind_cols(section_nbr) |>
      tidyr::unite("section", section, title, sep = ". ", remove = FALSE) |>
      dplyr::select(file, code, language, section, title, document) |>
      dplyr::bind_cols(folders) |>
      dplyr::mutate(
        languageweb = base::tolower(language),
        section = stringr::str_replace(section, "^\\. ", "")
      ) |>
      dplyr::mutate(
        languageweb = dplyr::case_when(
          languageweb %in% c("us","gb") ~ "en",
          TRUE ~ languageweb
        ),
        prefix = stringr::str_remove_all(
          stringr::str_extract_all(section, "^[0-9]+\\.", simplify = TRUE),
          "\\."
        )
      ) |>
      dplyr::mutate(
        complement = base::max(base::nchar(prefix))
      ) |>
      dplyr::mutate(
        complement = complement - base::nchar(prefix)
      ) |>
      dplyr::mutate(
        complement = purrr::map_chr(
          complement, function(x) base::paste(base::rep(0, x), collapse = "")
        )
      ) |>
      dplyr::mutate(
        weight = base::paste0(prefix, complement)
      ) |>
      dplyr::select(-complement)
    
    if (website == ""){
      textbook_structure$link <- base::as.character(NA)
    } else {
      textbook_structure <- textbook_structure |>
        dplyr::mutate(
          link = stringr::str_remove_all(
            folder, tree_name
          )
        ) |>
        dplyr::mutate(
          link = base::paste0(website, "/", languageweb, link)
        )
    }
    
    textbook_structure <- textbook_structure |>
      dplyr::arrange(folder)
    
  } else {
    textbook_structure <- tibble::tibble(
      file = base::character(0),
      code = base::character(0),
      language = base::character(0),
      section = base::character(0),
      title = base::character(0),
      document = base::character(0),
      folder = base::character(0),
      languageweb = base::character(0),
      prefix = base::character(0),
      weight = base::character(0),
      link = base::character(0)
    )
  }

  return(textbook_structure)
}

