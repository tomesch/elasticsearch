#' @export
getSource <- function (index, type = "_all", id, fields = NULL, source = TRUE,
                       realtime = TRUE, routing = NULL, preference = NULL,
                       refresh = FALSE, version = NULL, exists = FALSE, raw = FALSE,
                       validate.params = TRUE) {
  args = as.list(match.call())
  args[[1]] = NULL
  args[['get.source']] = TRUE
  do.call(get, args)
}
