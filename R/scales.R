#' Scale colors or fill for ggplot aesthetics
#'
#' @param palette name of the palette to be used. See \code{\link{nudz_palette}}
#' for all the available options
#' @param discrete logical determining if a discreate (finite values) scale
#' should be created or a continuous interpolation between colors of given palette
#' @param reversed shoudl hte order of the colors be reversed
#' @param n_colors selection of colors to be in the palette.
#' See \code{\link{nudz_palette}}
#' @param ... other parameter passed to \code{\link[ggplot2]{discrete_scale}}
#' or to the \code{\link{ggplot2::scale_color_gradientn}}
#'
#' @return ggplot2::discrete_scale or scale_color_gradientn with selected colors
#' @export
#'
#' @examples
scale_color_nudz <- function(palette = "main", discrete = TRUE, reversed = FALSE,
                             n_colors = c(), ...) {
  pal <- nudz_palette(palette = palette, reversed = reversed, discrete = discrete,
                      n_colors = n_colors)
  if (discrete) {
    discrete_scale("colour", paste0("nudz_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}


#' @describeIn scale_color_nudz Scale fill
#' @export
scale_fill_nudz <- function(palette = "main", discrete = TRUE, reversed = FALSE,
                            n_colors = c(), ...) {
  pal <- nudz_palette(palette = palette, reverse = reversed, discrete = FALSE,
                      n_colors = n_colors)
  if (discrete) { #fills and colors are the same if discrete
    discrete_scale("fill", paste0("nudz_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}
