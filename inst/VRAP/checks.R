## trycatch.W.E from demo(error.catching)
## Martin Maechler
## Copyright The R Core Team

tryCatch.W.E <- function(expr) {
  W <- NULL
  w.handler <- function(w){ ## warning handler
    W <<- w
    invokeRestart("muffleWarning")
  }
  list(value = withCallingHandlers(tryCatch(expr, error = function(e) e),
                                   warning = w.handler),
       warning = W)
}

## check for valid integer representations, silently
## TRUE : as.integer will return an integer equal to the represented value
## FALSE : as.integer will return an integer not equal to the represented
##         value, or an error or warning will be emitted

integer.rep <- function(xl) {
  res <- list()
  for (xn in names(xl)) {
    x <- xl[[xn]]
    r <- tryCatch.W.E(
      as.integer(x) == x        
    )
    res[xn] <- !is.na(r$value) & !inherits(r$value,"error") &
      is.null(r$warning) & r$value
  }
  res
}

numeric.rep <- function(xl) {
  res <- list()
  for (xn in names(xl)) {
    x <- xl[[xn]]
    r <- tryCatch.W.E(
      as.numeric(x)
    )
    res[xn] <- !is.na(r$value) & !inherits(r$value,"error") &
      is.null(r$warning)
  }
  res
}
