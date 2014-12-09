#' Deletes data in an index.
#'
#' \code{Delete_index} Deletes data in an index.
#'
#' @param index A string representing the index
#' @param timeout A string representing the timeout
#' @param masterTimeout A string representing the masterTimeout
#'
#' @examples
#' index("twitter", "tweet")
#' index("twitter", "_all")
#'
#' @references
#' \url{http://www.elasticsearch.org/guide/en/elasticsearch/client/javascript-api/current/api-reference.html#api-indices-delete}
#'
#' @export
Delete_index <- function (index, timeout = "1m", masterTimeout) {
  if (missing(index)) {
    stop()
  } else {
    url = getOption("res_url")
    path = paste(index, collapse = ",")
    args = list(timeout = timeout, masterTimeout = masterTimeout)

    if (validate.params) {
      ValidateArgs(args)
    }

    args = PrepareArgs(args)
    url = httr::modify_url(url, "path" = path, "args" = args)
    res = httr::DELETE(url)
    httr::stop_for_status(res)
    FormatESResult(res, raw)
  }
}

