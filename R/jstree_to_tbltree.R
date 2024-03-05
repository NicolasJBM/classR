#' @name jstree_to_tbltree
#' @title Transform a JSON tree into a table tree
#' @author Nicolas Mangin
#' @description Function to transforms a document hierarchical classification into a tibble and update all document-related information
#' @param jstree List. Document list as a classification tree.
#' @param documents Tibble.
#' @param document_types Tibble.
#' @return Tibble. List of documents with positions extracted from the classification tree and additional variables gathered from the document list.
#' @importFrom dplyr bind_rows
#' @importFrom dplyr case_when
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr starts_with
#' @importFrom purrr map
#' @importFrom purrr map_chr
#' @importFrom stringr str_detect
#' @importFrom tibble as_tibble
#' @importFrom tibble tibble
#' @importFrom tidyr replace_na
#' @importFrom tidyr unite
#' @importFrom tidyr unnest
#' @export


jstree_to_tbltree <- function(jstree, documents, document_types){
  
  data <- NULL
  text <- NULL
  children <- NULL
  level_0 <- NULL
  rank_0 <- NULL
  rank_1 <- NULL
  title <- NULL
  type <- NULL
  translations <- NULL
  path <- NULL
  position <- NULL
  
  
  list_to_dataframe <- function(x,y){
    if (base::length(x) > 0 & base::is.list(x)){
      df <- base::lapply(x, function(y){
        y <- y[base::intersect(base::names(y), c("text","icon","data","children"))]
        if ("children" %in% names(y)) y else y$children <- base::list()
        return(y)
      })
      df <- tibble::as_tibble(t(base::do.call(cbind, df))) |>
        dplyr::mutate(file = purrr::map_chr(data, function(x) if (base::length(x$file) == 1) x$file else base::as.character(NA))) |>
        dplyr::select(text, file, children)
    } else {
      df <- tibble::tibble(text = base::as.character(""), file = base::as.character(""), children = base::as.list(""))
    }
    df <- dplyr::mutate(df, rank = base::seq_len(base::nrow(df))) |>
      dplyr::mutate(rank = purrr::map_chr(rank, complete_position, y=3)) |>
      dplyr::select(text, file, rank, children) |>
      dplyr::mutate(
        text = base::as.character(text),
        file = base::as.character(file),
        rank = base::as.character(rank)
      ) |>
      dplyr::mutate(text = dplyr::case_when(
        base::is.na(file) ~ text,
        TRUE ~ file
      )) |>
      dplyr::select(-file)
    base::names(df) <- c(base::paste0("level_", y), base::paste0("rank_", y),"children")
    df
  }
  
  tbltree <- list_to_dataframe(jstree[1:2], 0) |>
    dplyr::mutate(children = purrr::map(children, list_to_dataframe, y = 1)) |>
    tidyr::unnest("children")
  
  for (i in 2:9){
    tbltree <- tbltree |>
      dplyr::mutate(children = purrr::map(children, list_to_dataframe, y = i)) |>
      tidyr::unnest("children")
  }
  
  tbltree <- base::as.data.frame(tbltree)
  
  for (i in 1:base::length(tbltree)){
    z <- unlist(tbltree[,i])
    z[z==""] <- NA
    z[stringr::str_detect(z, "fa fa-")] <- NA
    tbltree[,i] <- z
  }
  
  tbltree <- tbltree[,apply(tbltree, 2, function(x) mean(is.na(x)) < 1)]
  tbltree <- tbltree[,apply(tbltree, 2, function(x) length(unique(x)) > 1)]
  tbltree <- dplyr::filter(tbltree, level_0 != "Unclassified")
  
  files <- dplyr::select(tbltree, dplyr::starts_with("level_")) |>
    dplyr::mutate(path = base::as.character(NA), file = base::as.character(NA))
  for (i in 1:base::nrow(files)){
    tmpfile <- utils::tail(base::as.character(stats::na.omit(base::unlist(files[i,]))), n = 1)
    tmppath <- base::paste(base::as.character(stats::na.omit(base::unlist(files[i,]))), collapse = "/")
    files$file[i] <- tmpfile
    files$path[i] <- tmppath
  }
  
  unclassified <- base::setdiff(base::unique(documents$file), files$file)
  unclassified <- tibble::tibble(
    level_0 = "Unclassified",
    rank_0 = "03",
    level_1 = unclassified,
    rank_1 = base::seq_len(base::length(unclassified)),
    file = base::setdiff(base::unique(documents$file), files$file)
  ) |>
    dplyr::mutate(rank_1 = purrr::map_chr(rank_1, complete_position, 9)) |>
    dplyr::mutate(
      path = base::paste("Unclassified", file, sep = "/"),
      position = base::paste(rank_0, rank_1, sep = "-")
    ) |>
    dplyr::select(-rank_0, -rank_1)
  
  positions <- dplyr::select(tbltree, dplyr::starts_with("rank_"))
  positions <- tidyr::unite(positions, "position", base::names(positions), sep = "-")
  
  files$position <- positions$position
  tbltree <- files |>
    dplyr::bind_rows(unclassified)
  
  tbltree <- dplyr::bind_rows(
    tbltree, tibble::tibble(
      position = c("01","02","03"),
      path = c("Included","Excluded","Unclassified")
    )) |>
    dplyr::left_join(
      dplyr::select(documents, file, title, type, translations),
      by = "file"
    ) |>
    dplyr::left_join(
      dplyr::select(document_types, type, icon),
      by = "type"
    ) |>
    dplyr::select(file, path, position, title, type, icon, translations) |>
    tidyr::replace_na(base::list(icon = "folder-open"))
  return(tbltree)
}

