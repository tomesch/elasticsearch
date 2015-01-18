#' indices.delete
#'
#' Delete an index in Elasticsearch
#'
#' \code{Delete_index} Deletes data in an index.
#' @param index String The name of the index
#' @param timeout Date, Number Explicit operation timeout
#' @param master_timeout Date, Number Specify timeout for connection to master
#' @examples
#' index("twitter", "tweet")
#' index("twitter", "_all")
#'
#' @references
#' \url{http://www.elasticsearch.org/guide/en/elasticsearch/client/javascript-api/current/api-reference.html#api-indices-delete}
#'
#' @export
indices.delete <- function (client, ...) {
  UseMethod("indices.delete", client)
}

#' @rdname indices.delete
#' @export
indices.delete.elasticsearch <- function (client, index = "_all", timeout = NULL, master_timeout = NULL, raw = FALSE, validate_params = TRUE) {
  path = paste(index, collapse = ",")

  args = as.list(match.call())
  args = removeNonURLARgs(args)
  if (validate_params) {
    validateParams(args)
  }
  args = prepareArgs(args)

  url = httr::modify_url(client$url, "path" = path, "query" = args)
  res = httr::DELETE(url)
  httr::stop_for_status(res)

  formatESResult(res, raw)
}
