
#' @rdname stat_km
#' @export

StatKm <- ggproto("StatKm", Stat,

  compute_group = function(data, scales, se = TRUE, trans = "identity", firstx = 0, firsty = 1, ...) {

    sf <- survfit(Surv(data$time, data$status) ~ 1, se.fit = se, ...)
    trans <- scales::as.trans(trans)$trans
    #x <- sf$time
    if(is.null(sf$surv)) {
      x <- rep(sf$time, 2)
      sf$surv <- rep(1, length(x))
    }

    x <- c(firstx, sf$time)
    y <- trans(c(firsty, sf$surv))

    step <- dostep(x, y)

    se <- all(exists("upper", where = sf), exists("lower", where = sf))

    if(se){

      ymin <- trans(c(1, sf$lower))
      ymax <- trans(c(1, sf$upper))

      minstep <- dostep(x, ymin, yfix = y)
      maxstep <- dostep(x, ymax, yfix = y)

      df.out <- data.frame(time = step$x, survival = step$y,
                           ymin = minstep$y,
                           ymax = maxstep$y)
    } else df.out <- data.frame(time = step$x, survival = step$y)

    sf.df <- data.frame(time = sf$time,
               n.risk = sf$n.risk,
               n.censor = sf$n.censor,
               n.event = sf$n.event)

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
#' @param xstart,ystart the starting point for the survival curves. By default,
#'   the plot program obeys tradition by having the plot start at (0,1).
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


dostep <- function(x, y, yfix = NULL) {
  keep <- is.finite(x) & is.finite(y)
  if (!any(keep))
    return()
  if (!all(keep)) {
    x <- x[keep]
    y <- y[keep]
  }
  n <- length(x)
  if (n == 1)
    list(x = x, y = y)
  else if (n == 2)
    list(x = x[c(1, 2, 2)], y = y[c(1, 1, 2)])
  else {
    temp <- rle(y)$lengths
    if(!is.null(yfix)) {
      temp2 <- rle(yfix)$lengths
      if(length(temp2) > length(temp)) temp <- temp2
    }

    drops <- 1 + cumsum(temp[-length(temp)])
    if (n %in% drops) {
      xrep <- c(x[1], rep(x[drops], each = 2))
      yrep <- rep(y[c(1, drops)], c(rep(2, length(drops)),
                                    1))
    }
    else {
      xrep <- c(x[1], rep(x[drops], each = 2), x[n])
      yrep <- c(rep(y[c(1, drops)], each = 2))
    }
    list(x = xrep, y = yrep)
  }
}

