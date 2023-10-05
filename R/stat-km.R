
#' @importFrom ggplot2 layer aes ggproto
#' @import scales
#' @importFrom survival Surv survfit.formula
#' @importFrom grid pointsGrob nullGrob unit gpar gList

#' @rdname stat_km
#' @export

StatKm <- ggplot2::ggproto("StatKm", Stat,

  compute_group = function(data, scales, trans = "identity", firstx = 0, firsty = 1,
                           type = "kaplan-meier", start.time = 0) {

    sf <- survival::survfit.formula(survival::Surv(data$time, data$status) ~ 1, se.fit = FALSE,
                                    type = type, start.time = start.time)

    transloc <- scales::as.trans(trans)$trans

    if(is.null(sf$surv)) {
      x <- rep(sf$time, 2)
      sf$surv <- rep(1, length(x))
    }

    x <- c(firstx, sf$time)
    y <- transloc(c(firsty, sf$surv))
    y[y == -Inf] <- min(y[is.finite(y)])
    y[y == Inf] <- max(y[is.finite(y)])

    step <- dostep(x, y)
    df.out <- data.frame(time = step$x, survival = step$y)

    df.out

  },

  default_aes = ggplot2::aes(y = ..survival.., x = ..time..),
  required_aes = c("time", "status"),
  dropped_aes = c("time", "status")


)

## need to create a different stat for kmticks



#' @importFrom ggplot2 layer aes ggproto
#' @import scales
#' @importFrom survival Surv survfit.formula
#' @importFrom grid pointsGrob nullGrob unit gpar gList

#' @rdname stat_kmband
#' @export

StatKmband <- ggplot2::ggproto("StatKmband", Stat,

                               compute_group = function(data, scales, trans = "identity", firstx = 0, firsty = 1,
                                                        type = "kaplan-meier", error = "tsiatis", conf.type = "log",
                                                        conf.lower = "usual", start.time = 0, conf.int = 0.95) {

                                 sf <- survival::survfit.formula(survival::Surv(data$time, data$status) ~ 1, se.fit = TRUE,
                                                                 type = type, error = error, conf.type = conf.type,
                                                                 conf.lower = conf.lower, start.time = start.time, conf.int = conf.int)

                                 transloc <- scales::as.trans(trans)$trans
                                 #x <- sf$time
                                 if(is.null(sf$surv)) {
                                   x <- rep(sf$time, 2)
                                   sf$surv <- rep(1, length(x))
                                 }

                                 x <- c(firstx, sf$time)
                                 y <- transloc(c(firsty, sf$surv))
                                 y[y == -Inf] <- min(y[is.finite(y)])
                                 y[y == Inf] <- max(y[is.finite(y)])

                                 ymin <- transloc(c(firsty, sf$lower))
                                 ymax <- transloc(c(firsty, sf$upper))

                                 ymin[ymin == -Inf] <- min(ymin[is.finite(ymin)])
                                 ymin[ymin == Inf] <- max(ymin[is.finite(ymin)])
                                 ymax[ymax == -Inf] <- min(ymax[is.finite(ymax)])
                                 ymax[ymax == Inf] <- max(ymax[is.finite(ymax)])


                                 minstep <- dostep(x, ymin)
                                 maxstep <- dostep(x, ymax)

                                 a1 <- merge_steps(minstep, maxstep)

                                 df.out <- data.frame(time = a1[[1]]$x,
                                                      lower = a1[[1]]$y,
                                                      upper = a1[[2]]$y)

                                 df.out

                               },

                               default_aes = ggplot2::aes(ymin = ..lower.., ymax = ..upper.., x = ..time..),
                               required_aes = c("time", "status"),
                               dropped_aes = c("time", "status")


)


#' Adds a Kaplan Meier Estimate of Survival
#'
#' @section Aesthetics:
#' \code{stat_km} understands the following aesthetics (required aesthetics
#' are in bold):
#' \itemize{
#'   \item \strong{\code{time}} The survival times
#'   \item \strong{\code{status}} The censoring indicator, see \link[survival]{Surv} for more information.
#'   \item \code{alpha}
#'   \item \code{color}
#'   \item \code{linetype}
#'   \item \code{size}
#' }
#'
#' @inheritParams ggplot2::stat_identity
#' @param trans Transformation to apply to the survival probabilities. Defaults
#'   to "identity". Other options include "event", "cumhaz", "cloglog", or
#'   define your own using \link{trans_new}.
#' @param firstx,firsty the starting point for the survival curves. By default,
#'   the plot program obeys tradition by having the plot start at (0,1).
#' @param ... Other arguments passed to \code{survival::survfit.formula}
#' @return a data.frame with additional columns: \item{x}{x in data}
#'   \item{y}{Kaplan-Meier Survival Estimate at x}
#' @export
#'
#' @rdname stat_km
#' @details
#'
#' This stat is for computing the confidence intervals for the Kaplan-Meier survival estimate for
#' right-censored data. It requires the aesthetic mapping \code{x} for the
#' observation times and \code{status} which indicates the event status,
#' 0=alive, 1=dead or 1/2 (2=death). Logical status is not supported.
#'
#'
#' @examples
#' sex <- rbinom(250, 1, .5)
#' df <- data.frame(time = exp(rnorm(250, mean = sex)), status = rbinom(250, 1, .75), sex = sex)
#' ggplot(df, aes(time = time, status = status, color = factor(sex))) +
#'  stat_km()
#'
#' ## Examples illustrating the options passed to survfit.formula
#'
#' p1 <- ggplot(df, aes(time = time, status = status))
#' p1 + stat_km()
#' p1 + stat_km(trans = "cumhaz")
#' # cloglog plots also log transform the time axis
#' p1 + stat_km(trans = "cloglog") + scale_x_log10()
#' p1 + stat_km(type = "fleming-harrington")
#' p1 + stat_km(start.time = 5)
#'

stat_km <- function(mapping = NULL, data = NULL, geom = "km",
                    position = "identity", show.legend = NA, inherit.aes = TRUE,
                    se = TRUE, trans = "identity", firstx = 0, firsty = 1,
                    type = "kaplan-meier", start.time = 0) {
  ggplot2::layer(
    stat = StatKm,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(trans = trans, firstx = firstx, firsty = firsty,
                  type = type, start.time = start.time)
  )

}



#' Adds confidence bands to a Kaplan Meier Estimate of Survival
#'
#' @section Aesthetics:
#' \code{stat_kmband} understands the following aesthetics (required aesthetics
#' are in bold):
#' \itemize{
#'   \item \strong{\code{time}} The survival times
#'   \item \strong{\code{status}} The censoring indicator, see \link[survival]{Surv} for more information.
#'   \item \code{alpha}
#'   \item \code{color}
#'   \item \code{linetype}
#'   \item \code{size}
#' }
#'
#' @inheritParams ggplot2::stat_identity
#' @param \code{conf.int} to control significance level which is 0.95 by default
#' @param trans Transformation to apply to the survival probabilities. Defaults
#'   to "identity". Other options include "event", "cumhaz", "cloglog", or
#'   define your own using \link{trans_new}.
#' @param firstx,firsty the starting point for the survival curves. By default,
#'   the plot program obeys tradition by having the plot start at (0,1).
#' @param ... Other arguments passed to \code{survival::survfit.formula}
#' @return a data.frame with additional columns: \item{x}{x in data}
#'   \item{ymin}{Lower confidence
#'   limit of KM curve, if \code{se = TRUE}} \item{ymax}{Upper confidence limit
#'   of KM curve}
#' @export
#'
#' @rdname stat_kmband
#' @details
#'
#' This stat is for computing the confidence intervals for the Kaplan-Meier survival estimate for
#' right-censored data. It requires the aesthetic mapping \code{x} for the
#' observation times and \code{status} which indicates the event status,
#' 0=alive, 1=dead or 1/2 (2=death). Logical status is not supported.
#'
#' @examples
#' sex <- rbinom(250, 1, .5)
#' df <- data.frame(time = exp(rnorm(250, mean = sex)), status = rbinom(250, 1, .75), sex = sex)
#' ggplot(df, aes(time = time, status = status, color = factor(sex))) +
#'  stat_km()
#'
#' ## Examples illustrating the options passed to survfit.formula
#'
#' p1 <- ggplot(df, aes(time = time, status = status))
#' p1 + stat_km() + stat_kmband(conf.int = .99)
#' p1 + stat_km() + stat_kmband(error = "tsiatis")
#' p1 + stat_km() + stat_km(conf.type = "log-log")
#'

stat_kmband <- function(mapping = NULL, data = NULL, geom = "kmband",
                    position = "identity", show.legend = NA, inherit.aes = TRUE,
                    trans = "identity", firstx = 0, firsty = 1,
                    type = "kaplan-meier", error = "tsiatis", conf.type = "log",
                    conf.lower = "usual", start.time = 0, conf.int = 0.95) {
  ggplot2::layer(
    stat = StatKmband,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(trans = trans, firstx = firstx, firsty = firsty,
                  type = type, error = error, conf.type = conf.type,
                  conf.lower = conf.lower, start.time = start.time, conf.int = conf.int)
  )

}



#' Compute locations for tick marks
#'
#' Tick marks are plotted where there are censoring times that are also not event times
#'
#' @rdname stat_kmticks
#' @export

StatKmticks <- ggplot2::ggproto("StatKmticks", Stat,

                  compute_group = function(data, scales, trans = "identity", ...) {

                    sf <- survival::survfit.formula(survival::Surv(data$time, data$status) ~ 1, se.fit = FALSE, ...)
                    trans <- scales::as.trans(trans)$trans

                    sf.df <- data.frame(time = sf$time,
                                        survival = trans(sf$surv),
                                        n.risk = sf$n.risk,
                                        n.censor = sf$n.censor,
                                        n.event = sf$n.event)

                    sf.df

                  },

                  default_aes = aes(y = ..survival.., x = ..time..),
                  required_aes = c("time", "status"),
                  dropped_aes = c("time", "status")


)



#' Adds tick marks to a Kaplan Meier Estimate of Survival
#'
#' @section Aesthetics:
#' \code{stat_kmticks} understands the following aesthetics (required aesthetics
#' are in bold):
#' \itemize{
#'   \item \strong{\code{time}} The survival times
#'   \item \strong{\code{status}} The censoring indicator, see \link[survival]{Surv} for more information.
#'   \item \code{alpha}
#'   \item \code{color}
#'   \item \code{linetype}
#'   \item \code{size}
#' }
#'
#' @seealso \link{stat_km}
#' @inheritParams ggplot2::stat_identity
#' @param trans Transformation to apply to the survival probabilities. Defaults
#'   to "identity". Other options include "event", "cumhaz", "cloglog", or
#'   define your own using \link{trans_new}.
#' @param ... Other arguments passed to \code{survival::survfit.formula}
#' @return a data.frame with additional columns: \item{x}{x in data}
#'   \item{y}{Kaplan-Meier Survival Estimate at x}
#' @export
#' @rdname stat_kmticks
#'
#' @details
#'
#' This stat is for computing the tick marks for a Kaplan-Meier survival estimate for
#' right-censored data. The tick marks will appear at each censoring time which is also
#' not a death time, which is the default for \link{plot.survfit}.
#' It requires the aesthetic mapping \code{x} for the
#' observation times and \code{status} which indicates the event status,
#' normally 0=alive, 1=dead. Other choices are TRUE/FALSE (TRUE = death) or 1/2
#' (2=death).
#'
#' @examples
#' sex <- rbinom(250, 1, .5)
#' df <- data.frame(time = exp(rnorm(250, mean = sex)), status = rbinom(250, 1, .75), sex = sex)
#' ggplot(df, aes(time = time, status = status, color = factor(sex))) +
#'  stat_km() + stat_kmticks()
#'


stat_kmticks <- function(mapping = NULL, data = NULL, geom = "kmticks",
                    position = "identity", show.legend = NA, inherit.aes = TRUE, trans, ...) {
  ggplot2::layer(
    stat = StatKmticks,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(...)
  )

}


