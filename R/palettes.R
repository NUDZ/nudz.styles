NUDZ_PALLETES <- list(
  `main` = nudz_colors('red', 'yellow', 'light_blue', 'dark_blue')
)

#' Creates palette to be used in scales or elsewhere
#'
#' @param palette name of the palette to interpolate.
#' * __main__ Main red blue and dark blue palette
#'
#' Defaults to "main"
#' @param n_colors either how many colors from the palette to interpolate or
#' indices of colors  which to interpolate from the palette. If a single number
#' (n) then colors 1...n will be selected. If multiple numbers are passed
#' then the colors at given indices are selected. if empty, all colors are
#' used in defining the palette. Defaults to empty.
#'
#' @param reverse logical determining if the order of the colors should be
#' reversed
#' @param discrete should the palette be only discrete values
#' @param ... optional parameters passed to colorRampPalette if discrete is set
#' to FALSE
#'
#' @return vector of hex colors if discrete is TRUE and colorRamp object if FALSE.
#' Prepares the color interpolation object to be used in construction of your own
#' @export
#' @md
#'
#' @examples
nudz_palette <- function(palette = "main", reverse = FALSE, n_colors = c(),
                         discrete = TRUE, ...){
  if(!(palette %in% names(NUDZ_PALLETES))) return(NULL)
  pal <- NUDZ_PALLETES[[palette]]
  if(length(n_colors) == 1) pal <- pal[1:n_colors]
  if(length(n_colors) > 1) pal <- pal[n_colors]
  if(reverse) pal <- rev(pal)
  if(discrete) return(function(i){return(unname(pal[1:i]))}) #the unname is importnat
  # as otherwise the levels of the plot are being fit to the names in the colors
  # eg. red is being fit to level red int he plot
  return(colorRampPalette(pal, ...))
}
