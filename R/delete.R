#' Delete a document from an index based on its id
#'
#' \code{delete} deletes a document from an index based on its id.
#'
#' @param index A string representing the index under wi
#' @param type A string representing the index under wi
#' @param id A string representing the index under wi
#'
#' @examples
#' delete("twitter", "tweet", 1)
#'
#' @references
#' \url{http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/docs-delete.html}
#'
#' @export
delete <- function (index, type, id, version = NULL, routing = NULL,
                    parent = NULL, replication = "sync", refresh = FALSE,
                    timeout = "1m", consistency = NULL, raw = FALSE) {
  if (missing(index) || missing(type) || missing(id)) {
    stop()
  }
  else {
    url = getOption("res_url")

    path = paste(index, type, id, sep="/")
    args = list(version = version, routing = routing, parent = parent,
                refresh = refresh, timeout = timeout, replication = replication,
                consistency = consistency)

    validate_args(args)

    url = httr::modify_url(url, "path" = path, "query" = args)

    res <- httr::DELETE(url)
    httr::stop_for_status(res)

    format_res(res, raw)
  }
}
