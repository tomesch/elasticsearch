#' Deletes data in an index.
#'
#' \code{Delete_index} Deletes data in an index.
#'
#' @param index A string representing the index
#' @param timeout A string representing the value of the timeout
#'
#' @examples
#' index("twitter", "tweet")
#' index("twitter", "_all")
#'
#' @references
#' \url{http://www.elasticsearch.org/guide/en/elasticsearch/client/javascript-api/current/api-reference.html#api-indices-delete}
#'
#' @export
indices.delete <- function (client, index = "_all", raw = FALSE) {
  UseMethod("indices.delete", client)
}

#' @rdname indices.delete
#' @export
indices.delete.elasticsearch <- function (client, index = "_all", raw = FALSE) {
  path = paste(index, collapse = ",")

  url = httr::modify_url(client$url, "path" = path)
  res = httr::DELETE(url)
  httr::stop_for_status(res)

  formatESResult(res, raw)
}
