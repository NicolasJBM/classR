#' @name complete_position
#' @title Enforce constant lenght position
#' @author Nicolas Mangin
#' @description Add 0 to a number to force a constant length string.
#' @param x Integer. Position.
#' @param y Integer. Number of characters
#' @return Character. Position.
#' @export

complete_position <- function(x,y) base::paste(c(base::rep(0, y-base::nchar(x)), x), collapse = "")
