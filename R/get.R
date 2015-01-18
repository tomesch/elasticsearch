#' get
#'
#' Get a typed JSON document from the index based on its id.
#'
#' \code{get} searches a document in an index based on its id and returns it.
#'
#' @param client
#' @param index String The name of the index
#' @param type String The type of the document (use _all to fetch the first document matching the ID across all types)
#' @param id String The document ID
#' @param fields String, String[] A comma-separated list of fields to return in the response
#' @param source String, String[], Logical True or false to return the _source field or not, or a list of fields to return
#' @param source_include String, String[], Logical A list of fields to extract and return from the _source field
#' @param source_exclude String, String[], Logical A list of fields to exclude from the returned _source field
#' @param parent String The ID of the parent document
#' @param realtime Logical Specify whether to perform the operation in realtime or search mode
#' @param routing String Specific routing value
#' @param preference String Specify the node or shard the operation should be performed on (default: random)
#' @param refresh Logical Refresh the shard containing the document before performing the operation
#' @param version Number Explicit version number for concurrency control
#' @param version_type String Specific version type
#'
#' @examples
#' get("twitter", id="1")
#' get("twitter", "tweet", "1")
#' get("twitter", "tweet", "1", 'c("name", "date")')
#' get("twitter", "tweet", "1", 'c("name", "date")', TRUE)
#'
#' @references
#' \url{http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/docs-get.html}
#' @export get
get <- function (client, ...) {
  UseMethod("get", client)
}

#' @rdname get
#' @export
get.elasticsearch <- function (client, index, type = "_all", id, fields = NULL, source = TRUE,
                               source_include = NULL, parent = NULL, source_exclude = NULL, realtime = TRUE, routing = NULL, preference = "random",
                               refresh = FALSE, version = NULL, version_type = NULL, exists = FALSE, raw = FALSE,
                               validate_params = TRUE, get_source = FALSE) {
  if (missing(index) || missing(id)) {
    stop()
  }

  path = paste(index, paste(type, collapse = ","), id, sep = "/")
  if (get_source) {
    path = paste(path, '_source', sep = "/")
  }

  args = as.list(match.call())
  args = removeNonURLARgs(args)
  if (validate_params) {
    validateParams(args)
  }
  args = prepareArgs(args)

  url = httr::modify_url(client$url, "path" = path, "query" = args)

  if (exists) {
    res = httr::HEAD(url)
    if (res['status_code'] == 404) {
      return(FALSE)
    } else if (res['status_code'] == 200) {
      return(TRUE)
    }
  } else {
    res = httr::GET(url)
    httr::stop_for_status(res)
  }
  formatESResult(res, raw)
}
