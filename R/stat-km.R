
#' @rdname stat_km
#' @export

StatKm <- ggproto("StatKm", Stat,

  compute_group = function(data, scales, se = TRUE, trans = "identity", ...) {

    sf <- survfit(Surv(data$time, data$status) ~ 1, se.fit = se, ...)
    trans <- scales::as.trans(trans)$trans
    x <- sf$time
    if(is.null(sf$surv) || length(sf$surv) == 1){
      x <- rep(sf$time, 2)
      sf$surv <- rep(1, length(x))
    }
    y <- trans(sf$surv)

    se <- all(exists("upper", where = sf), exists("lower", where = sf))


    if(se){

      ymin <- trans(sf$lower)
      ymax <- trans(sf$upper)

      df.out <- data.frame(time = x, survival = y,
                           n.risk = sf$n.risk,
                           n.censor = sf$n.censor, n.event = sf$n.event,
                           ymin = ymin,
                           ymax = ymax,
                           se = sf$std.err)
    } else df.out <- data.frame(time = x, survival = y, n.risk = sf$n.risk,
                                n.censor = sf$n.censor, n.event = sf$n.event)

    df.out

  },

  default_aes = aes(y = ..survival.., x = ..time..),
  required_aes = c("time", "status")


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
#' @param se display confidence interval around KM curve? (TRUE by default, use
#'   \code{conf.int} to control significance level which is 0.95 by default)
#' @param trans Transformation to apply to the survival probabilities. Defaults
#'   to "identity". Other options include "event", "cumhaz", "cloglog", or
#'   define your own using \link{trans_new}.
#' @param ... Other arguments passed to \code{survival::survfit.formula}
#' @return a data.frame with additional columns: \item{x}{x in data}
#'   \item{y}{Kaplan-Meier Survival Estimate at x} \item{ymin}{Lower confidence
#'   limit of KM curve, if \code{se = TRUE}} \item{ymax}{Upper confidence limit
#'   of KM curve, if \code{se = FALSE}}
#' @export
#'
#' @details
#'
#' This stat is for computing the Kaplan-Meier survival estimate for
#' right-censored data. It requires the aesthetic mapping \code{x} for the
#' observation times and \code{status} which indicates the event status,
#' normally 0=alive, 1=dead. Other choices are TRUE/FALSE (TRUE = death) or 1/2
#' (2=death).
#'
#' @examples
#' sex <- rbinom(250, 1, .5)
#' df <- data.frame(time = exp(rnorm(250, mean = sex)), status = rbinom(250, 1, .75), sex = sex)
#' ggplot(df, aes(time, status = status, color = factor(sex))) +
#'  stat_km()
#'
#' ## Examples illustrating the options passed to survfit.formula
#'
#' p1 <- ggplot(df, aes(x = Surv(time, status)))
#' p1 + stat_km(conf.int = .99)
#' p1 + stat_km(trans = "cumhaz")
#' # cloglog plots also log transform the time axis
#' p1 + stat_km(trans = "cloglog") + scale_x_log10()
#' p1 + stat_km(type = "fleming-harrington")
#' p1 + stat_km(error = "tsiatis")
#' p1 + stat_km(conf.type = "log-log")
#' p1 + stat_km(start.time = 200)
#'

stat_km <- function(mapping = NULL, data = NULL, geom = "km",
                    position = "identity", show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    stat = StatKm,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(...)
  )

}



cumhaz_trans <- function(){

  trans <- function(x){
    -log(x)
  }

  inv <- function(x){
    exp(-x)
  }

  trans_new("cumhaz",
            trans,
            inv,
            log_breaks(base = exp(1)),
            domain = c(0, Inf) ## The domain over which the transformation is valued
  )
}


event_trans <- function(){

  trans <- function(x){
    1-x
  }

  inv <- function(x){
    1-x
  }

  trans_new("event",
            trans,
            inv,
            pretty_breaks(),
            domain = c(0, 1) ## The domain over which the transformation is valued
  )
}


cloglog_trans <- function(){

  trans <- function(x){
    log(-log(x))
  }

  inv <- function(x){
    exp(-exp(x))
  }

  trans_new("cloglog",
            trans,
            inv,
            pretty_breaks(),
            domain = c(-Inf, Inf) ## The domain over which the transformation is valued
  )
}
