#' update
#'
#' Update parts of a document
#' \code{update} updates a document based on a script provided.
#'
#' @param index
#' @param type
#' @param id
#' @param body Json
#' @param consistency String Explicit write consistency setting for the operation
#' @param fields String, String[], Logical A comma-separated list of fields to return in the response
#' @param lang String The script language (default: groovy)
#' @param parent String ID of the parent document
#' @param refresh Logical Refresh the index after performing the operation
#' @param replication String Specific replication type
#' @param retry_on_conflict Number Specify how many times should the operation be retried when a conflict occurs (default: 0)
#' @param routing String Specific routing value
#' @param timeout Number Explicit operation timeout
#' @param timestamp Date Explicit timestamp for the document
#' @param ttl Duration Expiration time for the document
#' @param version Number Explicit version number for concurrency control
#' @param version_type String Specific version type
#' @examples
#' update("twitter", "tweet", 1)
#'
#' @references
#' \url{http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/docs-update.html}
#'
#' @export
update <- function (client, ...) {
  UseMethod("update", client)
}

#' @rdname update
#' @export
update.elasticsearch <- function (client, index, type, id, body, routing = NULL, parent = NULL,
                                  timeout = "1m", refresh = FALSE, fields = NULL,
                                  version = NULL, version_type = NULL, validate_params = TRUE, raw = FALSE, consistency = NULL, lang = "groovy", replication = "sync", retry_on_conflict = NULL, timestamp = NULL, ttl = NULL) {
  if (missing(index) || missing(type) || missing(id)) {
    stop()
  } else {
    path = paste(index, type, id, "_update", sep="/")

    args = as.list(match.call())
    args = removeNonURLARgs(args)
    if (validate_params) {
      validateParams(args)
    }
    args = prepareArgs(args)

    url = httr::modify_url(client$url, "path" = path, "query" = args)
    res <- httr::POST(url, body=body, encode = "json")
    httr::stop_for_status(res)

    formatESResult(res, raw)
  }
}
