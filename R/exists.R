#' @export
exists <- function (client, index, type = "_all", id, fields = NULL, realtime = TRUE,
                    routing = NULL, preference = NULL, refresh = FALSE,
                    version = NULL, validate.params = TRUE) {
  UseMethod("exists", client)
}

#' @rdname exists
#' @export
exists.elasticsearch <- function (client, index, type = "_all", id, fields = NULL, realtime = TRUE,
                    routing = NULL, preference = NULL, refresh = FALSE,
                    version = NULL, validate.params = TRUE) {
  args = as.list(match.call())
  args[[1]] = NULL
  args[['exists']] = TRUE
  do.call(get, args)
}
