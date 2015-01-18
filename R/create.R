#' create
#'
#' Adds a typed JSON document in a specific index, making it searchable. If a document with the same index, type, and id already exists, an error will occur.
#' @param client
#' @param index String The name of the index
#' @param type String The type of the document
#' @param id String Document ID
#' @param body Json The document
#' @param version Number Explicit version number for concurrency control
#' @param versionType String Specific version type
#' @param routing Date Explicit operation timeout
#' @param parent String ID of the parent document
#' @param timestamp Date Explicit timestamp for the document
#' @param ttl Date Expiration time for the document
#' @param refresh Logical Refresh the index after performing the operation
#' @param timeout Date Explicit operation timeout
#' @param raw
#' @param validate.params
#'
#' @export
create <- function (client, ...) {
  UseMethod("create", client)
}

#' @rdname create
#' @export
create.elasticsearch <- function (client, index, type, id, body, consistency = NULL, replication = "sync", version = NULL,
                                  version_type = NULL, routing = NULL,
                                  parent = NULL, timestamp = NULL, ttl = NULL, refresh = FALSE,
                                  timeout = "1m", raw = FALSE, validate_params = TRUE) {
  args = as.list(match.call())
  args[[1]] = NULL
  args[['op_type']] = 'create'
  do.call(elasticsearch::index, args)
}
