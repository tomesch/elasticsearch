#' exists
#'
#' Returns a boolean indicating whether or not a given document exists.
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
#' @export
exists <- function (client, ...) {
  UseMethod("exists", client)
}

#' @rdname exists
#' @export
exists.elasticsearch <- function (client, index, type = "_all", id, fields = NULL, source = TRUE,
                                  source_include = NULL, parent = NULL, source_exclude = NULL, realtime = TRUE, routing = NULL, preference = "random",
                                  refresh = FALSE, version = NULL, version_type = NULL, raw = FALSE,
                                  validate_params = TRUE, get.source = FALSE) {
  args = as.list(match.call())
  args[[1]] = NULL
  args[['exists']] = TRUE
  do.call(get, args)
}
