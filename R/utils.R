

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

  n2 <- s1$x[vapply(s1$x, function(x) !x %in% s2$x, TRUE)]

  ns2 <- s2
  wats <- vapply(n2, function(x){

    t1 <- s2$x[x > s2$x]
    if(length(t1) < 1) {
      return(NA)
    } else {
      max(which(s2$x == max(t1, na.rm = TRUE)))
    }

  }, integer(1))
  wats <- wats[!is.na(wats)]

  ns2$x <- append(ns2$x, n2)
  ns2$y <- append(ns2$y, ns2$y[wats])

  res2 <- list(x = sort(ns2$x), y = ns2$y[order(ns2$x)])

  n1 <- res2$x[vapply(res2$x, function(x) !x %in% s1$x, TRUE)]

  ns1 <- s1
  wats <- vapply(n1, function(x) {
    t1 <- s1$x[x > s1$x]
    if(length(t1) < 1) {
      return(NA)
    } else {
      max(which(s1$x == max(t1, na.rm = TRUE)))
    }
  }, integer(1))

  ns1$x <- append(ns1$x, n1)
  ns1$y <- append(ns1$y, ns1$y[wats])


  res1 <- list(x = sort(ns1$x), y = ns1$y[order(ns1$x)])

  list(s1 = res1, s2 = res2)


}

