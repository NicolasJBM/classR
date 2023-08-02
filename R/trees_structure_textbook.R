#' @name trees_structure_textbook
#' @title Create a textbook structure
#' @author Nicolas Mangin
#' @description Function creating the textbook structure associated with a tree.
#' @param tree list.
#' @param tree_name Character. Name of the tree.
#' @param website Character. Address of the website where the textbook is hosted.
#' @return Tibble with all necessary information about the pages of the textbook.
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_cols
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_all
#' @importFrom dplyr select
#' @importFrom dplyr starts_with
#' @importFrom purrr map_chr
#' @importFrom stringr str_count
#' @importFrom stringr str_detect
#' @importFrom stringr str_remove
#' @importFrom stringr str_remove_all
#' @importFrom stringr str_replace
#' @importFrom stringr str_split
#' @importFrom tibble tibble
#' @importFrom tidyr separate
#' @importFrom tidyr unite
#' @export


trees_structure_textbook <- function(tree, tree_name = "", website = ""){

  position <- NULL
  type <- NULL
  LEVEL_0 <- NULL
  LEV1 <- NULL
  folder <- NULL
  language <- NULL
  title <- NULL
  document <- NULL
  section <- NULL
  complement <- NULL
  link <- NULL
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
  
  if (base::nrow(files) > base::length(stringr::str_split(files$position[1], "\\.", simplify = TRUE))){
    
    files$position <- stringr::str_remove_all(files$position, "^1.")
    
    positions <- files |>
      dplyr::select(position) |>
      tidyr::separate(
        position, into = base::paste0("LEV", base::seq_len(levnbr)), sep = "\\."
      ) |>
      dplyr::mutate_all(base::as.numeric) |>
      dplyr::mutate_all(function(x) x-1) |>
      dplyr::mutate_all(function(x) base::replace(x, x<=0, 0)) |>
      dplyr::mutate(LEV1 = LEV1+1)
    
    for (i in base::seq_len(base::length(positions))) positions[i,i] <- 0
    
    if (base::nrow(positions) > base::length(positions)){
      coordinates <- base::list()
      coordinates[[1]] <- base::rep(0, levnbr)
      for (i in 2:base::nrow(positions)){
        tmp1 <- base::as.numeric(positions[i-1,])
        tmp2 <- base::as.numeric(positions[i,])
        tmp3 <- tmp2 - tmp1
        tmp3add <- tmp3
        tmp3add[tmp3<1] <- 0
        tmp3add[tmp3>1] <- 1
        incr <- 0
        for (j in 2:base::length(tmp3add)){
          incr <- base::max(incr, tmp3add[j-1])
          if (incr > 0) tmp3add[j] <- 0
        }
        tmp3rem <- tmp3
        tmp3rem[tmp3>=0] <- 0
        tmp4 <- coordinates[[i-1]] + tmp3rem + tmp3add
        tmp4 <- base::replace(tmp4, tmp4<0, 0)
        coordinates[[i]] <- tmp4
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
          folders[row,col] <- base::paste(c("section", tmpid), collapse = "")
        } else folders[row,col] <- NA
      }
    }
    
    section_nbr <- positions |>
      tidyr::unite("section", base::names(positions), na.rm = TRUE, sep = "")
    
    folders <- tidyr::unite(
      folders, "folder", base::names(folders), sep = "/", na.rm = TRUE
    )
    
    textbook_structure <- files |>
      dplyr::bind_cols(section_nbr) |>
      dplyr::select(file, code, language, section, title, document) |>
      dplyr::bind_cols(folders) |>
      dplyr::mutate(
        language = base::tolower(language),
        section = stringr::str_replace(section, "^\\. ", "")
      ) |>
      dplyr::mutate(
        complement = base::max(base::nchar(section))
      ) |>
      dplyr::mutate(
        complement = complement - base::nchar(section)
      ) |>
      dplyr::mutate(
        complement = purrr::map_chr(
          complement, function(x) base::paste(base::rep(0, x), collapse = "")
        )
      ) |>
      dplyr::mutate(
        order = base::paste0(section, complement)
      ) |>
      dplyr::select(-complement)
    
    if (website == ""){
      textbook_structure$link <- base::as.character(NA)
    } else {
      textbook_structure <- textbook_structure |>
        dplyr::mutate(
          link = base::paste0(website, "_", language, "/pages/", order, ".html")
        )
    }
    
    textbook_structure <- textbook_structure |>
      dplyr::select(-folder) |>
      dplyr::arrange(order) 
    
  } else {
    textbook_structure <- tibble::tibble(
      file = base::character(0),
      code = base::character(0),
      language = base::character(0),
      section = base::character(0),
      title = base::character(0),
      document = base::character(0),
      order = base::character(0),
      link = base::character(0)
    )
  }

  return(textbook_structure)
}

