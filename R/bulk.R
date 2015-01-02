#' @export
bulk <- function (client, requests, index, type, consistency = NULL, replication = "sync", routing = NULL, refresh = FALSE,
                   timeout = "1m", raw = FALSE, validate.params = TRUE) {
  UseMethod("bulk", client)
}

#' @rdname bulk
#' @export
bulk.elasticsearch <- function (client, requests, index, type, consistency = NULL, replication = "sync", routing = NULL, refresh = FALSE,
                                timeout = "1m", raw = FALSE, validate.params = TRUE) {
  if (missing(requests)) {
    stop()
  } else {
    path = "_bulk"
    if (!missing(index) && !missing(type)) {
      path = paste(index, type, path, sep="/")
    } else if (!missing(index)) {
      path = paste(index, path, sep="/")
    }
    args = list(consistency = consistency, replication = replication, routing = routing, timeout = timeout)

    if (validate.params) {
      validateArgs(args)
    }

    args = prepareArgs(args)

    url = httr::modify_url(client$url, "path" = path, "query" = args)

    res = httr::POST(url, body=requests, encode = "json")
    httr::stop_for_status(res)

    formatESResult(res, raw)
  }
}

