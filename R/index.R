#' index
#'
#' Stores a typed JSON document in an index, making it searchable. When the id param is not set, a unique id will be auto-generated. When you specify an id either a new document will be created, or an existing document will be updated. To enforce "put-if-absent" behavior set the op_type to "create" or use the create() method.
#'
#' \code{index} adds or updates a document in a specific index.
#'
#' @param index String The name of the index
#' @param type String The type of the document
#' @param id String Document ID
#' @param document Json
#' @param consistency String Explicit write consistency setting for the operation
#' @param replication String Specific replication type
#' @param version Number Explicit version number for concurrency control
#' @param version_type String Specific version type
#' @param routing String Specific routing value
#' @param timeout Date Explicit operation timeout
#' @param refresh Logical Refresh the index after performing the operation
#' @param ttl Date Expiration time for the document
#' @param parent String ID of the parent document
#' @param timestamp String Explicit timestamp for the document
#'
#' @examples
#' index("twitter", "tweet", 1, '{"test_index": "text_index"}')
#' index("twitter", "tweet", body='{"test_index": "text_index"}')
#'
#' @references
#' \url{http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/docs-index_.html}
#' @export
index <- function (client, ...) {
  UseMethod("index", client)
}

#' @rdname index
#' @export
index.elasticsearch <- function (client, index, type, id, body, consistency = NULL, replication = "sync", version = NULL,
                                 version_type = NULL, op_type = NULL, routing = NULL,
                                 parent = NULL, timestamp = NULL, ttl = NULL, refresh = FALSE,
                                 timeout = "1m", raw = FALSE, validate_params = TRUE) {
  if (missing(index) || missing(type) || missing(body)) {
    stop()
  } else {
    path = paste(index, type, sep="/")
    if (!missing(id)) {
      path = paste(path, id, sep="/")
    }

    args = as.list(match.call())
    args = removeNonURLARgs(args)
    if (validate_params) {
      validateParams(args)
    }
    args = prepareArgs(args)

    url = httr::modify_url(client$url, "path" = path, "query" = args)

    if (!missing(id)) {
      res = httr::PUT(url, body=body, encode = "json")
    } else {
      res = httr::POST(url, body=body, encode = "json")
    }
    httr::stop_for_status(res)

    formatESResult(res, raw)
  }
}
