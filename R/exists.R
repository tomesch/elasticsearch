#' @export
exists <- function (index, type = "_all", id, fields = NULL, realtime = TRUE,
                    routing = NULL, preference = NULL, refresh = FALSE,
                    version = NULL, raw = FALSE, validate.params = TRUE) {
  args = as.list(match.call())
  args[[1]] = NULL
  args[['exists']] = TRUE
  do.call(Get, args)
}
