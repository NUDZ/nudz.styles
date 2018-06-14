NUDZ_COLORS_HEX <- c(
  `red` = '',
  `yellow` = '',
  `light_blue` = '',
  `dark_blue` = ''
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
nudz_colors <- function(colors = NULL){
  if(is.null(colors)) return(NUDZ_COLORS_HEX)
  if(colors %in% names(NUDZ_COLORS_HEX)){
    return(NUDZ_COLORS_HEX[colors])
  }
  warning('This color doesn\'t exist')
  return(NULL)
}
