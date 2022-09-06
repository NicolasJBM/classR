#' @name trees_create_list_levels
#' @title Transform a tree tibble into a tree JSON
#' @author Nicolas Mangin
#' @description Recursive function used in trees_tibble_to_json to transform a list of documents into a json hierarchy using the position argument to organize the hierarchy (remove custom tags in the process)
#' @param x Tibble. Classification tree for documents.
#' @param depth Integer. 
#' @param level Integer. 
#' @return List. Multi-layered hierarchical classification equivalent to a json file structure
#' @seealso trees_tibble_to_json
#' @seealso trees_fill_list_level
#' @export


trees_create_list_levels <- function(x, depth, level = 1){
  
  # The first algorithm applies only to the first level
  if (base::nrow(x) > 0 & level == 1){
    
    # Split the table into a list of sub-tables for each section of that level
    children_list <- base::split(x, x[,base::paste0("LEV",level)])
    
    # Apply the same function at lower levels
    children_list <- base::lapply(
      children_list, classR::trees_create_list_levels,
      level = level+1, depth = depth
    )
    
    simplified <- base::list()
    for (i in base::seq_len(base::length(children_list))){
      simplified[[i]] <- children_list[[i]]
    }
    
    y <- classR::trees_fill_list_level(root = NULL, children = simplified)
    
    # This second algorithm applies on non-empty sub-levels
  } else if (base::nrow(x) > 0 & level > 1 & level <= depth){
    
    # Identify document at the root of the level and those at sub-levels
    root <- x[x[,base::paste0("LEV",level)] == 0,]
    children <- x[x[,base::paste0("LEV",level)] != 0,]
    
    # If there are children, these are put in a sub-list processed later
    if (base::nrow(children) > 0){
      
      children_list <- base::split(
        children, children[,base::paste0("LEV",level)]
      )
      
      children_list <- base::lapply(
        children_list, trees_create_list_levels,
        level = level+1, depth = depth
      )
      
      simplified <- base::list()
      for (i in base::seq_len(base::length(children_list))){
        simplified[[i]] <- children_list[[i]]
      } 
      
      if (base::nrow(root) > 0){
        y <- classR::trees_fill_list_level(
          root = root, children = simplified
        )
      } else y <- simplified
      
      # Otherwise, the children sub-list is empty
    } else {
      
      y <- classR::trees_fill_list_level(root = root, children = NULL)
      
    }
    # the next algorithm is necessary to stop after the last leaf
  } else {
    
    root <- x
    
    if (base::nrow(x) > 0){
      
      y <- classR::trees_fill_list_level(root = root, children = NULL)
      
    } else {
      
      y <- classR::trees_fill_list_level(root = NULL, children = NULL)
      
    }
  }
  return(y)
}

