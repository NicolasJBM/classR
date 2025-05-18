#' @name trees_structure_textbook
#' @title Create a textbook structure
#' @author Nicolas Mangin
#' @description Function creating the textbook structure associated with a tree.
#' @param tree Tibble.
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

  LEV1 <- NULL
  position <- NULL
  section <- NULL
  title <- NULL
  type <- NULL

  tree_name <- stringr::str_remove(tree_name, ".RData$")
  website <- stringr::str_remove(website, "/$")

  levnbr <- stringr::str_count(tree$position[1], "-")

  files <- tree |>
    dplyr::filter(
      stringr::str_detect(position, "^[0]+1-"),
      type == "Page",
      !base::is.na(file)
    )
  
  language <- base::tolower(base::unique(stringr::str_extract(files$file, "[A-Z]{2}(?=\\.Rmd)")))
  
  if (base::nrow(files) > base::length(stringr::str_split(files$position[1], "-", simplify = TRUE))){
    
    files$position <- stringr::str_remove_all(files$position, "^[0]+1-")
    
    positions <- files |>
      dplyr::select(position) |>
      tidyr::separate(
        position, into = base::paste0("LEV", base::seq_len(levnbr)), sep = "-"
      ) |>
      dplyr::mutate_all(base::as.numeric) |>
      dplyr::mutate_all(function(x) x-1) |>
      dplyr::mutate_all(function(x) base::replace(x, x<=0, 0)) |>
      dplyr::mutate(LEV1 = LEV1+1)
    
    for (i in base::seq_len(base::length(positions))) positions[i,i] <- 0
    
    sections <- base::list()
    sections[[1]] <- base::rep(0, levnbr)
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
      tmp4 <- sections[[i-1]] + tmp3rem + tmp3add
      tmp4 <- base::replace(tmp4, tmp4<0, 0)
      sections[[i]] <- tmp4
    }
    
    sections <- base::unlist(base::lapply(
      sections, function(x) base::paste(base::lapply(x, classR::complete_position, 2), collapse = "-")
    ))
    
    textbook_structure <- files |>
      dplyr::mutate(order = sections) |>
      dplyr::arrange(order) |>
      dplyr::mutate(
        section = stringr::str_replace_all(order, "^0|-0+", "-"),
        section = stringr::str_remove_all(section, "^-|-{2,}|-$"),
        section = stringr::str_remove_all(section, "0"),
        section = stringr::str_replace_all(section, "-", "."),
        order = stringr::str_remove_all(order, "-")
      ) |>
      dplyr::select(file, section, title, order)
    
    
    if (website == ""){
      textbook_structure$link <- base::as.character(NA)
    } else {
      textbook_structure <- textbook_structure |>
        dplyr::mutate(
          link = base::paste0(website, "_",language,"/pages/", order, ".html")
        )
    }
    
  } else {
    textbook_structure <- tibble::tibble(
      file = base::character(0),
      section = base::character(0),
      title = base::character(0),
      order = base::character(0),
      link = base::character(0)
    )
  }

  return(textbook_structure)
}

