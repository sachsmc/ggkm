
#' Display Kaplan Meier Curve
#'
#' Kaplan Meier Curve
#'
#' @export
#' @rdname geom_km


GeomKm <- ggplot2::ggproto("GeomKm", Geom,

                  draw_group = function(data, scales, coordinates, ...) {

                    path <- transform(data, alpha = NA)
                    GeomPath$draw_panel(path, scales, coordinates)

                  },

                  required_aes = c("x", "y"),
                  default_aes = ggplot2::aes(colour="black", fill="grey60", size=.75,
                                    linetype=1, weight=1, alpha=0.4, direction = "hv"),

                  draw_key = draw_key_path

)


#' Display Kaplan Meier Curve
#'
#' Kaplan Meier Curve
#'
#' @export
#' @rdname geom_kmband


GeomKmband <- ggplot2::ggproto("GeomKmband", Geom,

                           draw_group = function(data, scales, coordinates, ...) {

                             ribbon <- transform(data, colour = NA)
                             path <- transform(data, alpha = NA)

                             GeomRibbon$draw_group(ribbon, scales, coordinates)

                           },

                           required_aes = c("x", "ymin", "ymax"),
                           default_aes = ggplot2::aes(colour="black", fill="grey60", size=.75,
                                                      linetype=1, weight=1, alpha=0.4),

                           draw_key = draw_key_smooth

)


#' Display tick marks on a Kaplan Meier curve
#'
#' Ticks are plotted at censoring times that are also not event times
#'
#' @export
#' @rdname geom_kmticks

GeomKmticks <- ggplot2::ggproto("GeomKmticks", Geom,

                       draw_group = function(data, scales, coordinates, ...) {

                         showpoints <- data$n.censor > 0 & data$n.event == 0

                         coordsp <- coordinates$transform(data, scales)[showpoints, , drop = FALSE]

                         if(nrow(coordsp) == 0){
                           grid::nullGrob()
                         } else {
                         grid::pointsGrob(
                           coordsp$x, coordsp$y,
                           pch = coordsp$shape,
                           size = grid::unit(coordsp$size, "char"),
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
                       default_aes = ggplot2::aes(
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
#' ggplot(df, aes(time = time, status = status, color = factor(sex))) + geom_km()

geom_km <- function(mapping = NULL, data = NULL, stat = "km",
                     position = "identity", show.legend = NA,
                     inherit.aes = TRUE, na.rm = TRUE, ...) {
  ggplot2::layer(
    geom = GeomKm, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


#' Add confidence bands to a Kaplan-Meier survival curve
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
#' @seealso The default stat for this geom is \code{\link{stat_kmband}} see
#'   that documentation for more options to control the underlying statistical transformation.
#' @export
#' @examples
#' sex <- rbinom(250, 1, .5)
#' df <- data.frame(time = exp(rnorm(250, mean = sex)), status = rbinom(250, 1, .75), sex = sex)
#' ggplot(df, aes(time = time, status = status, color = factor(sex))) + geom_km() + geom_kmband()

geom_kmband <- function(mapping = NULL, data = NULL, stat = "kmband",
                    position = "identity", show.legend = NA,
                    inherit.aes = TRUE, na.rm = TRUE, ...) {
  ggplot2::layer(
    geom = GeomKmband, mapping = mapping, data = data, stat = stat,
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
#' @seealso The default stat for this geom is \code{\link{stat_kmticks}} see
#'   that documentation for more options to control the underlying statistical transformation.
#' @export
#' @rdname geom_kmticks
#' @examples
#' sex <- rbinom(250, 1, .5)
#' df <- data.frame(time = exp(rnorm(250, mean = sex)), status = rbinom(250, 1, .75), sex = sex)
#' ggplot(df, aes(time = time, status = status, color = factor(sex))) + geom_km() + geom_kmticks()

geom_kmticks <- function(mapping = NULL, data = NULL, stat = "kmticks",
                    position = "identity", show.legend = NA,
                    inherit.aes = TRUE, na.rm = TRUE, ...) {
  ggplot2::layer(
    geom = GeomKmticks, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
