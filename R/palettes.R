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

#' Scale colors for ggplot color aesthetic
#'
#' @param palette
#' @param discrete
#' @param reverse
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
scale_color_nudz <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- nudz_palette(palette = palette, reverse = reverse)
  if (discrete) {
    discrete_scale("colour", paste0("nudz_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}


#' Scale colors for ggplot fill aesthetics
#'
#' @param palette
#' @param discrete
#' @param reverse
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
scale_fill_nudz <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- nudz_palette(palette = palette, reverse = reverse)
  if (discrete) {
    discrete_scale("fill", paste0("nudz_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}
