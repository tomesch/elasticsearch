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
delete <- function (index, type, id, query = NULL, version = NULL, routing = NULL,
                    parent = NULL, replication = "sync", refresh = FALSE,
                    timeout = "1m", consistency = NULL, raw = FALSE,
                    validate.params = TRUE) {
  if (!is.null(query)) {
    # delete by query API
    url = getOption("res_url")

    if(missing(index) && missing(type)) {
      path = paste('_all', '_query', sep="/")
    } else if (missing(index)) {
      stop()
    } else if (missing(type)) {
      path = paste(paste(index, collapse = ","), '_query', sep="/")
    } else {
      path = paste(paste(index, collapse = ","), paste(type, collapse = ","), sep="/")
    }

    args = list(version = version, routing = routing, parent = parent,
                refresh = refresh, timeout = timeout, replication = replication,
                consistency = consistency)

    if (validate.params) {
      validateArgs(args)
    }

    args = prepareArgs(args)

    url = httr::modify_url(url, "path" = path, "query" = args)

    res <- httr::DELETE(url, body = query)
    httr::stop_for_status(res)

    formatESResult(res, raw)
  } else if(!missing(id) && !missing(index) && !missing(type)) {
    # delete by id API
    url = getOption("res_url")

    path = paste(index, type, id, sep="/")
    args = list(version = version, routing = routing, parent = parent,
                refresh = refresh, timeout = timeout, replication = replication,
                consistency = consistency)

    if (validate.params) {
      validateArgs(args)
    }

    args = prepareArgs(args)

    url = httr::modify_url(url, "path" = path, "query" = args)

    res <- httr::DELETE(url)
    httr::stop_for_status(res)

    formatESResult(res, raw)
  } else {
    stop()
  }
}
