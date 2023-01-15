#' @noRd
print_format <- function(x, p = FALSE){
  uu <- format(x, big.mark = " ", scientific = FALSE)
  if ( isTRUE(p) ){
    uu <- if ( x == 1 ) paste0(uu, " point") else { paste0(uu, " points") }
  }
  uu
}



