NUDZ_COLORS_HEX <- c(
  `red` = '#e10613',
  `yellow` = '#ffec00',
  `light_blue` = '#5bc3f0',
  `dark_blue` = '#164192',
  `light_grey` = "#e6e6e6",
  `dark_grey` = "#898989",
  `black` = "#000000",
  `white` = "#ffffff"
)

#' Returns HEX color from the NUDZ color list
#'
#' @param color vector of colors which are translated to their hex interpretations.
#' Possible values at shi moment are
#' * red
#' * yellow
#' * light_blue
#' * dark_blue
#' * light_grey
#' * dark_grey
#' * black
#' * white
#'
#' @return hex color
#' @export
#'
#' @examples
#' @md
#' nudz_colors('red')
#' nudz_colors(c('red', 'light_blue'))
nudz_colors <- function(...){
  colors <- c(...)
  if(is.null(colors)) return(NUDZ_COLORS_HEX)
  # Validation
  return(NUDZ_COLORS_HEX[colors])
}
