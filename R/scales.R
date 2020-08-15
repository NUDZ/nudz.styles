#' Scale colors or fill for ggplot aesthetics
#'
#' @param palette name of the palette to be used. See \code{\link{nudz_palette}}
#' for all the available options
#' @param discrete logical determining if a discreate (finite values) scale
#' should be created or a continuous interpolation between colors of given palette
#' @param reverse shoudl hte order of the colors be reversed
#' @param ... other parameter passed to \code{\link{ggplot2::discrete_scale}}
#' or to the \code{\link{ggplot2::scale_color_gradientn}}
#'
#' @return
#' @export
#'
#' @examples
scale_color_nudz <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- nudz_palette(palette = palette, reverse = reverse, discrete = discrete)
  if (discrete) {
    discrete_scale("colour", paste0("nudz_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}


#' @describeIn scale_color_nudz Scale fill
#' @export
scale_fill_nudz <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  if (discrete) { #fills and colors are the same if discrete
    return(scale_color_nudz(palette, discrete, reverse, ...))
  } else {
    pal <- nudz_palette(palette = palette, reverse = reverse, discrete = FALSE)
    scale_fill_gradientn(colours = pal(256), ...)
  }
}
