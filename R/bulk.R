#' bulk
#'
#' Perform many index/delete operations in a single API call
#' @param client ElasticsearchClient
#' @param body Json The request body
#' @param index String The default index for the operations
#' @param type String The default type for the operations
#' @param consistency String Explicit write consistency setting for the operation
#' @param replication String Explicit replication type setting
#' @param routing String Specific routing value
#' @param refresh Logical Refresh the index after performing the operation
#' @param timeout Explicit operation timeout
#' @param raw
#' @param validate.params
#' @export bulk
bulk <- function (client, ...) {
  UseMethod("bulk", client)
}

#' @rdname bulk
#' @export
bulk.elasticsearch <- function (client, body, index, type, consistency = NULL, replication = "sync", routing = NULL, refresh = FALSE,
                                timeout = "1m", raw = FALSE, validate_params = TRUE) {
  if (missing(body)) {
    stop()
  } else {
    path = "_bulk"
    if (!missing(index) && !missing(type)) {
      path = paste(index, type, path, sep="/")
    } else if (!missing(index)) {
      path = paste(index, path, sep="/")
    }

    args = as.list(match.call())
    args = removeNonURLARgs(args)
    if (validate_params) {
      validateParams(args)
    }
    args = prepareArgs(args)

    url = httr::modify_url(client$url, "path" = path, "query" = args)
    res = httr::POST(url, body = body, encode = "json")
    httr::stop_for_status(res)

    formatESResult(res, raw)
  }
}

