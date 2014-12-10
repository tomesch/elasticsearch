#' @export
create <- function (index, type, id, document, version = NULL,
                    version_type = NULL, routing = NULL,
                    parent = NULL, timestamp = NULL, ttl = NULL, refresh = FALSE,
                    timeout = "1m", raw = FALSE, validate.params = TRUE) {
  args = as.list(match.call())
  args[[1]] = NULL
  args[['op_type']] = 'create'
  do.call(elasticsearch::index, args)
}
