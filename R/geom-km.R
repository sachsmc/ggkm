
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
                      GeomPath$draw_panel(path, scales, coordinates)
                    )
                  },

                  required_aes = c("x", "y"),
                  default_aes = aes(colour="black", fill="grey60", size=.75, linetype=1, weight=1, alpha=0.4, direction = "hv"),

                  draw_key = draw_key_smooth

)


GeomKmticks <- ggproto("GeomKmticks", Geom,

                       draw_group = function(data, scales, coordinates, ...) {

                         showpoints <- data$n.censor > 0 & data$n.event == 0

                         coordsp <- coordinates$transform(data, scales)[showpoints, , drop = FALSE]

                         if(nrow(coordsp) == 0){
                           grid::nullGrob()
                         } else {
                         grid::pointsGrob(
                           coordsp$x, coordsp$y,
                           pch = coordsp$shape,
                           size = unit(coordsp$size, "char"),
                           gp = grid::gpar(
                             col = coordsp$colour,
                             fill = coordsp$fill,
                             alpha = coordsp$alpha
                           )
                         )
                         }

                       },

                       required_aes = c("x", "y"),
                       non_missing_aes = c("size", "shape"),
                       default_aes = aes(
                         shape = 3, colour = "black", size = .75,
                         alpha = 1, stroke = 0.5, fill = "black"
                       ),
                       draw_key = draw_key_point

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
                     inherit.aes = TRUE, na.rm = TRUE, ...) {
  layer(
    geom = GeomKm, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


#' Add tick marks to a Kaplan-Meier survival curve
#'
#' Adds tickmarks at the times when there are censored observations but no events
#'
#' @section Aesthetics:
#' \code{geom_kmticks} understands the following aesthetics (required aesthetics
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
#' ggplot(df, aes(x = time, status = status, color = factor(sex))) + geom_km() + geom_kmticks()

geom_kmticks <- function(mapping = NULL, data = NULL, stat = "km",
                    position = "identity", show.legend = NA,
                    inherit.aes = TRUE, na.rm = TRUE, ...) {
  layer(
    geom = GeomKmticks, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
