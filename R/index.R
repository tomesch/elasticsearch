#' Add or update a document in a specific index
#'
#' \code{index} adds or updates a document in a specific index.
#'
#' @param index A string representing the index
#' @param type A string representing the type
#' @param id A string representing the id
#' @param document A string representing the document
#' @param version A string representing the return of a version for each search hit.
#' @param version_type A string representing the version type.
#' @param op_type A string representing the operation type.
#' @param routing A string allowing to control the _routing aspect when indexing data and explicit routing control is required.
#' @param parent A string pointing to the parent type this child relates to.
#' @param timestamp A string indicating the value of the associated timestamp for this document.
#' @param ttl A string representing the time to live.
#' @param refresh A boolean that allows to explicitly refresh one or more index, making all operations performed since the last refresh.
#' @param timeout A string representing the value of the timeout
#' @param raw A boolean that indicates if the format of the response should be in json or not.
#' @param validate.params A boolean indicating the need to validate the passing parameters or not.
#'
#' @examples
#' index("twitter", "tweet", 1, '{"test_index": "text_index"}')
#' index("twitter", "tweet", document='{"test_index": "text_index"}')
#'
#' @references
#' \url{http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/docs-index_.html}
#' @export
index <- function (client, index, type, id, document, version = NULL,
                  version_type = NULL, op_type = NULL, routing = NULL,
                  parent = NULL, timestamp = NULL, ttl = NULL, refresh = FALSE,
                  timeout = "1m", raw = FALSE, validate.params = TRUE) {
  UseMethod("index", client)
}

#' @rdname index
#' @export
index.elasticsearch <- function (client, index, type, id, document, version = NULL,
                   version_type = NULL, op_type = NULL, routing = NULL,
                   parent = NULL, timestamp = NULL, ttl = NULL, refresh = FALSE,
                   timeout = "1m", raw = FALSE, validate.params = TRUE) {
  if (missing(index) || missing(type) || missing(document)) {
    stop()
  } else {
    path = paste(index, type, sep="/")
    if (!missing(id)) {
      path = paste(path, id, sep="/")
    }
    args = list(parent = parent, op_type = op_type, version = version,
                version_type = version_type, routing = routing,
                timestamp = timestamp, ttl = ttl, refresh = refresh,
                timeout = timeout)

    if (validate.params) {
      validateArgs(args)
    }

    args = prepareArgs(args)

    url = httr::modify_url(client$url, "path" = path, "query" = args)

    if (!missing(id)) {
      res = httr::PUT(url, body=document, encode = "json")
    } else {
      res = httr::POST(url, body=document, encode = "json")
    }
    httr::stop_for_status(res)

    formatESResult(res, raw)
  }
}
