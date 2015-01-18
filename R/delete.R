#' delete
#'
#' Delete a typed JSON document from a specific index based on its id.
#'
#' \code{delete} deletes a document from an index based on its id.
#'
#' @param index String The name of the index
#' @param type String The type of the document
#' @param id String The document ID
#' @param consistency String Specific write consistency setting for the operation
#' @param parent String ID of parent document
#' @param refresh Logical Refresh the index after performing the operation
#' @param replication String Specific replication type
#' @param routing String Specific routing value
#' @param timeout Date Explicit operation timeout
#' @param version Number Explicit version number for concurrency control
#' @param version_type String Specific version type
#'
#' @examples
#' delete("twitter", "tweet", 1)
#'
#' @references
#' \url{http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/docs-delete.html}
#'
#' @export delete
delete <- function (client, ...) {
  UseMethod("delete", client)
}

#' @rdname delete
#' @export
delete.elasticsearch <- function (client, index, type, id, query = NULL, version = NULL, version_type = NULL, routing = NULL,
                                  parent = NULL, replication = "sync", refresh = FALSE,
                                  timeout = "1m", consistency = NULL, raw = FALSE,
                                  validate_params = TRUE) {
  if (!is.null(query)) {
    # delete by query API
    if(missing(index) && missing(type)) {
      path = paste('_all', '_query', sep="/")
    } else if (missing(index)) {
      stop()
    } else if (missing(type)) {
      path = paste(paste(index, collapse = ","), '_query', sep="/")
    } else {
      path = paste(paste(index, collapse = ","), paste(type, collapse = ","), '_query', sep="/")
    }

    args = as.list(match.call())
    args = removeNonURLARgs(args)
    if (validate_params) {
      validateParams(args)
    }
    args = prepareArgs(args)

    url = httr::modify_url(client$url, "path" = path, "query" = args)
    res <- httr::DELETE(url)
    httr::stop_for_status(res)

    formatESResult(res, raw)
  } else if(!missing(id) && !missing(index) && !missing(type)) {
    # delete by id API
    path = paste(index, type, id, sep="/")

    args = as.list(match.call())
    args = removeNonURLARgs(args)
    if (validate_params) {
      validateParams(args)
    }
    args = prepareArgs(args)

    url = httr::modify_url(client$url, "path" = path, "query" = args)
    res <- httr::DELETE(url)
    httr::stop_for_status(res)

    formatESResult(res, raw)
  } else {
    stop()
  }
}
