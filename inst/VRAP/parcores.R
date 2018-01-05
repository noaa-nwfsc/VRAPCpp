lcores <- parallel::detectCores()

if (is.na(lcores) || lcores == 1) {
  parcores <- function() { 1 }
} else {
  parcores <- function() {lcores %/% 2}
}
