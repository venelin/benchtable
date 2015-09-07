#' Apply the function what on all lines specified by filter in benchdata
#' @param benchdata a data.table 
#' @param filter a list of elements with modes corresponding to the first key-columns of
#' benchdata
#' @param what a function(row) returning a list of statistics for a row of benchdata
#' @export
analyseBenchData <- function(benchdata, filter, what, ...) {
  t(apply(benchdata[filter], 1, what, ...))
}

#' Create a list of matrices with results from analyseBenchData as columns
#' @param benchdata a data.table with a key
#' @param filter a list of elements with modes corresponding to the first key-columns of
#' benchdata
#' @param what a function(row) returning a list of statistics for a row of benchdata
#' @param levels a vector with elements corresponding to the columns of the matrices. The
#' mode of this vector should correspond to the mode of a key-column in benchdata
#' 
#' @export
listBenchAnalysis <- function(benchdata, filter, what, levels) {
  an <- lapply(levels, function(level) {
    analyseBenchData(benchdata, c(filter, level), what)
  })
  names(an) <- as.character(levels)
  
  transposeAnForBoxPlot(an)
}

#' transpose an into a list of S matrices of length(levels) columns
#' @param an a named list of analysis returned by analyseBendData
#' @param levels character or numeric corresponding to box-plot-groups to be plotted along the x-axis
#' @param namesBoxes character corresponding to different boxes in each box-plot-group.
#' @return a named list of matrices suitable for bplListBenchAnalysis and vioplListBenchAnalysis 
#' @export
transposeAnForBoxPlot <- function(an, levels=names(an), namesBoxes=names(an[[1]][[1]])) {
  S <- lapply(namesBoxes, function(n) {
    maxlen <- max(sapply(as.character(levels), function(level) length(an[[level]])))
    sapply(as.character(levels), function(level) {
      sapply(1:maxlen, function(i) {
        if(i <= length(an[[level]])) {
          ifelse(length(an[[level]][[i]][[n]]), an[[level]][[i]][[n]], NA)
        } else {
          NA
        }
      })
    })
  })
  names(S) <- namesBoxes
  S  
}