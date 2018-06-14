NUDZ_PALLETES <- list(
  `main` = nudz_colors('red', 'light_blue', 'dark_blue')
)

#' Title
#'
#' @param palette
#' @param reverse
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
nudz_palette <- function(palette = "main", reverse = F, ...){
  if(palette %in% names(NUDZ_PALLETES)){
    pal <- NUDZ_PALLETES[[palette]]
    if (reverse) pal <- rev(pal)
    pal <- colorRampPalette(pal, ...)
    return(pal)
  } else {
    return(NULL)
  }
}
