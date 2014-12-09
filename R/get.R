#' Get a document from an index based on its id
#'
#' \code{get} searches a document in an index based on its id and returns it.
#'
#' @param index A string representing the index under wi
#' @param type A string representing the index under wi
#' @param id A string representing the index under wi
#' @param field A string representing the index under wi
#' @param raw A string representing the index under wi
#'
#' @examples
#' get("twitter", id="1")
#' get("twitter", "tweet", "1")
#' get("twitter", "tweet", "1", 'c("name", "date")')
#' get("twitter", "tweet", "1", 'c("name", "date")', TRUE)
#'
#' @references
#' \url{http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/docs-get.html}
#'
#' @export
get <- function (index, type = "_all", id, fields = NULL, realtime = TRUE,
                 routing = NULL, preference = NULL, refresh = FALSE,
                 version = NULL, exists = FALSE, raw = FALSE,
                 validate.params = TRUE) {
  if (missing(index) || missing(id)) {
    stop()
  }
  url = getOption("res_url")

  path = paste(index, type, id, sep = "/")

  if (!is.null(fields)) {
    fields = paste(fields, collapse = ",")
  }
  args = list(fields = fields, realtime = realtime, routing = routing,
              preference = preference, refresh = refresh, version = version)

  if (validate.params) {
    validateArgs(args)
  }

  args = prepareArgs(args)

  url = httr::modify_url(url, "path" = path, "query" = args)

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
