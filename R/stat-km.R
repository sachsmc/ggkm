
#' @importFrom ggplot2 layer aes ggproto
#' @import scales
#' @importFrom survival Surv survfit.formula
#' @importFrom grid pointsGrob nullGrob unit gpar gList

#' @rdname stat_km
#' @export

StatKm <- ggplot2::ggproto("StatKm", Stat,

  compute_group = function(data, scales, se = TRUE, trans = "identity", firstx = 0, firsty = 1,
                           type = "kaplan-meier", error = "tsiatis", conf.type = "log",
                           conf.lower = "usual", start.time = 0, conf.int = 0.95) {

    sf <- survival::survfit.formula(survival::Surv(data$time, data$status) ~ 1, se.fit = se,
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

    step <- dostep(x, y)

    se <- all(exists("upper", where = sf), exists("lower", where = sf))

    if(se){

      ymin <- transloc(c(firsty, sf$lower))
      ymax <- transloc(c(firsty, sf$upper))

      minstep <- dostep(x, ymin)
      maxstep <- dostep(x, ymax)

      a1 <- merge_steps(minstep, maxstep)
      a2 <- merge_steps(step, a1[[1]])
      a3 <- merge_steps(step, a1[[2]])

      df.out <- data.frame(time = a2[[1]]$x, survival = a2[[1]]$y,
                           ymin = a2[[2]]$y,
                           ymax = a3[[2]]$y)

    } else df.out <- data.frame(time = step$x, survival = step$y)

    df.out[rowSums(sapply(df.out, is.na)) == 0, ]

  },

  default_aes = ggplot2::aes(y = ..survival.., x = ..time..),
  required_aes = c("time", "status")


)

## need to create a different stat for kmticks




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
#' @param firstx,firsty the starting point for the survival curves. By default,
#'   the plot program obeys tradition by having the plot start at (0,1).
#' @param ... Other arguments passed to \code{survival::survfit.formula}
#' @return a data.frame with additional columns: \item{x}{x in data}
#'   \item{y}{Kaplan-Meier Survival Estimate at x} \item{ymin}{Lower confidence
#'   limit of KM curve, if \code{se = TRUE}} \item{ymax}{Upper confidence limit
#'   of KM curve, if \code{se = FALSE}}
#' @export
#'
#' @rdname stat_km
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
#' ggplot(df, aes(time = time, status = status, color = factor(sex))) +
#'  stat_km()
#'
#' ## Examples illustrating the options passed to survfit.formula
#'
#' p1 <- ggplot(df, aes(time = time, status = status))
#' p1 + stat_km(conf.int = .99)
#' p1 + stat_km(trans = "cumhaz")
#' # cloglog plots also log transform the time axis
#' p1 + stat_km(trans = "cloglog") + scale_x_log10()
#' p1 + stat_km(type = "fleming-harrington")
#' p1 + stat_km(error = "tsiatis")
#' p1 + stat_km(conf.type = "log-log")
#' p1 + stat_km(start.time = 5)
#'

stat_km <- function(mapping = NULL, data = NULL, geom = "km",
                    position = "identity", show.legend = NA, inherit.aes = TRUE,
                    se = TRUE, trans = "identity", firstx = 0, firsty = 1,
                    type = "kaplan-meier", error = "tsiatis", conf.type = "log",
                    conf.lower = "usual", start.time = 0, conf.int = 0.95) {
  ggplot2::layer(
    stat = StatKm,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(se = se, trans = trans, firstx = firstx, firsty = firsty,
                  type = type, error = error, conf.type = conf.type,
                  conf.lower = conf.lower, start.time = start.time, conf.int = conf.int)
  )

}





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
                  required_aes = c("time", "status")


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




cumhaz_trans <- function(){

  trans <- function(x){
    -log(x)
  }

  inv <- function(x){
    exp(-x)
  }

  scales::trans_new("cumhaz",
            trans,
            inv,
            scales::log_breaks(base = exp(1)),
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
            scales::pretty_breaks(),
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
            scales::pretty_breaks(),
            domain = c(-Inf, Inf) ## The domain over which the transformation is valued
  )
}


dostep <- function(x, y) {
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



merge_steps <- function(s1, s2) {

  n2 <- s1$x[sapply(s1$x, function(x) !x %in% s2$x)]

  ns2 <- s2
  wats <- vapply(n2, function(x) max(which(s2$x == max(s2$x[x > s2$x]))), 1L)

  ns2$x <- append(ns2$x, n2)
  ns2$y <- append(ns2$y, ns2$y[wats])

  res2 <- list(x = sort(ns2$x), y = ns2$y[order(ns2$x)])

  n1 <- res2$x[vapply(res2$x, function(x) !x %in% s1$x, TRUE)]

  ns1 <- s1
  wats <- vapply(n1, function(x) max(which(s1$x == max(s1$x[x > s1$x]))), 1L)

  ns1$x <- append(ns1$x, n1)
  ns1$y <- append(ns1$y, ns1$y[wats])


  res1 <- list(x = sort(ns1$x), y = ns1$y[order(ns1$x)])

  list(s1 = res1, s2 = res2)


}

