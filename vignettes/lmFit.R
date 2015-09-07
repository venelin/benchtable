lmFit <- function(p, ...) {
  lm(y~x, p[c('y', 'x')], ...)
}