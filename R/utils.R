mergeUrlArgs <- function (x) {
  sapply(unique(names(x)), function(z) unlist(x[names(x) == z], use.names=FALSE), simplify=FALSE)
}