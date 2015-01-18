#' indices.create
#'
#' Create an index in Elasticsearch.
#' @param index String The name of the index
#' @param timeout Date, Number Explicit operation timeout
#' @param master_timeout Date, Number Specify timeout for connection to master
#' @export
indices.create <- function (client, ...) {
  UseMethod("indices.create", client)
}

#' @rdname indices.create
#' @export
indices.create.elasticsearch <- function (client, index, timeout = NULL, master_timeout = NULL, raw = FALSE, validate_params = TRUE) {
  if (missing(index)) {
    stop()
  } else {
    path = paste(index, collapse = ",")

    args = as.list(match.call())
    args = removeNonURLARgs(args)
    if (validate_params) {
      validateParams(args)
    }
    args = prepareArgs(args)

    url = httr::modify_url(client$url, "path" = path, "query" = args)
    res = httr::PUT(url)
    httr::stop_for_status(res)

    formatESResult(res, raw)
  }
}
