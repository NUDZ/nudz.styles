NUDZ_PALLETES <- list(
  `main` = nudz_colors('red', 'yellow', 'light_blue', 'dark_blue', 'dark_grey'),
  `grey_and_blue` = nudz_colors('light_grey', 'light_blue', 'dark_blue', 'dark_grey'),
  `red_and_blue` = nudz_colors('red', 'light_blue', 'dark_blue'),
  `black_and_white` = nudz_colors('black', 'dark_grey', 'light_grey', 'white')
)

#' Creates palette to be used in scales or elsewhere
#'
#' @param palette name of the palette to interpolate.
#' * __main__ Main red blue and dark blue palette
#' * __grey_and_blue__ Combination of grey and blue colors
#' Defaults to "main"
#' @param n_colors either how many colors from the palette to interpolate or
#' indices of colors  which to interpolate from the palette. If a single number
#' (n) then colors 1...n will be selected. If multiple numbers are passed
#' then the colors at given indices are selected. if empty, all colors are
#' used in defining the palette. Defaults to empty.
#'
#' @param reversed logical determining if the order of the colors should be
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
nudz_palette <- function(palette = "main", reversed = FALSE, n_colors = c(),
                         discrete = TRUE, ...){
  if(!(palette %in% names(NUDZ_PALLETES))) return(NULL)
  pal <- NUDZ_PALLETES[[palette]]
  if(length(n_colors) == 1) pal <- pal[1:n_colors]
  if(length(n_colors) > 1) pal <- pal[n_colors]
  if(reversed) pal <- rev(pal)
  if(discrete) return(function(i){return(unname(pal[1:i]))}) #the unname is importnat
  # as otherwise the levels of the plot are being fit to the names in the colors
  # eg. red is being fit to level red int he plot
  return(colorRampPalette(pal, ...))
}


#' Visualizes the palette
#'
#' @param pal created nudz_palette to be visualized
#'
#' @return image of the palette
#' @export
#'
#' @examples
show_palette <- function(pal){
  colors <- pal(256)
  colors <- colors[!is.na(colors)]
  n <- length(colors)
  old <- par(mar = c(0.5, 0.5, 0.5, 0.5))
  on.exit(par(old))

  image(1:n, 1, as.matrix(1:n), col = colors,
        ylab = "", xaxt = "n", yaxt = "n", bty = "n")
}
