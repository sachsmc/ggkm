
#' Display Kaplan Meier Curve
#'
#' Kaplan Meier Curve
#'
#' @export
#' @rdname geom_km


GeomKm <- ggproto("GeomKm", Geom,

                  draw_group = function(data, scales, coordinates, ...) {

                    ribbon <- transform(data, colour = NA)
                    path <- transform(data, alpha = NA)

                    has_ribbon <- function(x) !is.null(x$ymax) && !is.null(x$ymin)

                    grid::gList(
                      if (has_ribbon(data)) GeomRibbon$draw_group(ribbon, scales, coordinates),
                      GeomStep$draw_panel(path, scales, coordinates)
                    )
                  },

                  required_aes = c("x", "y"),
                  default_aes = aes(colour="black", fill="grey60", size=.75, linetype=1, weight=1, alpha=0.4),

                  draw_key = draw_key_smooth

)


#' Add a Kaplan-Meier survival curve
#'
#' @section Aesthetics:
#' \code{geom_km} understands the following aesthetics (required aesthetics
#' are in bold):
#' \itemize{
#'   \item \strong{\code{x}} The survival/censoring times. This is automatically mapped by \link{stat_km}
#'   \item \strong{\code{y}} The survival probability estimates. This is automatically mapped by \link{stat_km}
#'   smallest level in sort order is assumed to be 0, with a warning
#'   \item \code{alpha}
#'   \item \code{color}
#'   \item \code{linetype}
#'   \item \code{size}
#' }
#'
#' @inheritParams ggplot2::geom_point
#' @seealso The default stat for this geom is \code{\link{stat_km}} see
#'   that documentation for more options to control the underlying statistical transformation.
#' @export
#' @examples
#' sex <- rbinom(250, 1, .5)
#' df <- data.frame(time = exp(rnorm(250, mean = sex)), status = rbinom(250, 1, .75), sex = sex)
#' ggplot(df, aes(x = time, status = status, color = factor(sex))) + geom_km()

geom_km <- function(mapping = NULL, data = NULL, stat = "km",
                     position = "identity", show.legend = NA,
                     inherit.aes = TRUE, ...) {
  layer(
    geom = GeomKm, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(...)
  )
}
