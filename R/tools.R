#' Shortcut for sapply(l, function(.) expr)
#' 
#' @param l a list
#' @param expr an expression that is evaluated to every element, ., of l, e.g. 5*.
#' 
#' @return eval(substitute(sapply(ll, function(.) expr)), list(ll=l))
#' 
#' @export
s. <- function(l, expr) {
  eval(substitute(sapply(ll, function(.) expr)), list(ll=l))
}

#' Shortcut for lapply(l, function(.) expr)
#' 
#' @param l a list
#' @param expr an expression that is applied to every element, ., of l, e.g. 5*.
#' 
#' @return eval(substitute(lapply(ll, function(.) expr)), list(ll=l))
#' 
#' @export
l. <- function(l, expr) {
  eval(substitute(lapply(ll, function(.) expr)), list(ll=l))
}

#' Construct a data.frame from its arguments and evaluate expression on every row
#' @param l1,l2,... two or more vector or list objects. A data.frame is constructed
#'  from these by `data.frame(l1, l2, ...)`.
#' @param expr any R expression that is evaluated on every row, ., of the data.frame.
#' @return eval(substitute(apply(dt, 1, function(.) expr)))
#' @export 
ll. <- function(l1=NULL, l2=NULL, expr, ...) {
  if(is.null(l1) & is.null(l2)) 
    dt <- data.table(...)
  else if(is.null(l1) & !is.null(l2))
    dt <- data.table(l2, ...)
  else if(!is.null(l1) & is.null(l2)) 
    dt <- data.table(l1, ...)
  else 
    dt <- data.table(l1, l2, ...)
  
  eval(substitute(apply(dt, 1, function(.) expr)))
}

#' Construct a data.frame from its arguments and evaluate expression on every row
#' @param l1,l2,... two or more vector or list objects. A data.frame is constructed
#'  from these `by data.frame(l1, l2, ...)`.
#' @param expr any R expression that is evaluated on every row, ., of the data.frame.
#' @return `eval(substitute(alply(dt, 1, function(.) expr)))``
#' @export 
lll. <- function(l1=NULL, l2=NULL, expr, ...) {
  if(is.null(l1) & is.null(l2)) 
    dt <- data.table(...)
  else if(is.null(l1) & !is.null(l2))
    dt <- data.table(l2, ...)
  else if(!is.null(l1) & is.null(l2)) 
    dt <- data.table(l1, ...)
  else 
    dt <- data.table(l1, l2, ...)
  
  eval(substitute(alply(dt, 1, function(.) expr)))
}

#' Shortcut for apply(x, 1, function(.) expr)
#' @param x something you can call apply on, i.e. a matrix, a data.frame a data.table...
#' @param expr expr any R expression that is evaluated on every row, ., of x
#' @return eval(substitute(apply(xx, 1, function(.) expr)), list(xx=x))
#' @export
a1. <- function(x, expr) {
  eval(substitute(apply(xx, 1, function(.) expr)), list(xx=x))
}

#' Shortcut for apply(x, 2, function(.) expr)
#' @param x something you can call apply on, i.e. a matrix, a data.frame a data.table...
#' @param expr expr any R expression that is evaluated on every row, ., of x
#' @return eval(substitute(apply(xx, 2, function(.) expr)), list(xx=x))
#' @export
a2. <- function(x, expr) {
  eval(substitute(apply(xx, 2, function(.) expr)), list(xx=x))
}
