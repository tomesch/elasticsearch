#' info
#'
#' Get basic info from the current cluster.
#'
#' @param client ElasticsearchClient
#' @param raw Logical
#'
#' @examples
#' info(es)
#' info(es, raw = TRUE)
#'
#' @export
info <- function (client, ...) {
  UseMethod("info", client)
}

#' @rdname info
#' @export
info.elasticsearch <- function (client, raw = FALSE) {
  res <- httr::GET(client$url)
  httr::stop_for_status(res)

  formatESResult(res, raw)
}
