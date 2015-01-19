#' mget
#'
#' Get multiple documents based on an index, type (optional) and ids.
#'
#' @param client
#' @param index String The name of the index
#' @param type String The type of the document (use _all to fetch the first document matching the ID across all types)
#' @param fields String, String[] A comma-separated list of fields to return in the response
#' @param source String, String[], Logical True or false to return the _source field or not, or a list of fields to return
#' @param source_include String, String[], Logical A list of fields to extract and return from the _source field
#' @param source_exclude String, String[], Logical A list of fields to exclude from the returned _source field
#' @param realtime Logical Specify whether to perform the operation in realtime or search mode
#' @param preference String Specify the node or shard the operation should be performed on (default: random)
#' @param refresh Logical Refresh the shard containing the document before performing the operation
#' @param routing String Specific routing value
#'
#'
#' @references
#' \url{http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/docs-multi-get.html}
#' @export
mget <- function (client, ...) {
  UseMethod("mget", client)
}

#' @rdname mget
#' @export
mget.elasticsearch <- function (client, index, type = "_all", body, fields, source,
                               source_include, source_exclude, realtime, preference,
                               refresh, routing, raw = FALSE, validate_params = TRUE) {
  if (missing(index)) {
    path = "_mget"
  } else {
    path = paste(index, paste(type, collapse = ","), '_mget', sep = "/")
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

