NUDZ_COLORS_HEX <- c(
  `red` = '#e10613',
  `yellow` = '#ffec00',
  `light_blue` = '#5bc3f0',
  `dark_blue` = '#164192'
)

#' Returns HEX color from the NUDZ color list
#'
#' @param color
#'
#' @return hex color
#' @export
#'
#' @examples
#' colors <- nudz_colors('red')
#'
#'
nudz_colors <- function(...){
  colors <- c(...)
  if(is.null(colors)) return(NUDZ_COLORS_HEX)
  # Validation
  return(NUDZ_COLORS_HEX[colors])
}
