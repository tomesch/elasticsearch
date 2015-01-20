#' scroll
#'
#' Scroll a search request (retrieve the next set of results) after specifying the scroll parameter in a search() call.
#'
#' @param client ElasticsearchClient
#' @param scroll_id String The scroll ID
#' @param scroll Duration Specify how long a consistent view of the index should be maintained for scrolled search
#' @param raw Logical
#'
#' @examples
#' scroll(es, scroll_id = "c2Nhbjs2OzM0NDg1ODpzRlBLc0FXNlNyNm5JWUc1", scroll = "1m")
#'
#' @export
scroll <- function (client, ...) {
  UseMethod("scroll", client)
}

#' @rdname scroll
#' @export
scroll.elasticsearch <- function (client, scroll_id, scroll, validate_params = TRUE, raw = FALSE) {
  if (missing(scroll_id)) {
    stop()
  }
  path = "_search/scroll"

  args = as.list(match.call())
  args = removeNonURLARgs(args)
  if (validate_params) {
    validateParams(args)
  }
  args = prepareArgs(args)

  url = httr::modify_url(client$url, "path" = path, "query" = args)
  res = httr::GET(url)
  httr::stop_for_status(res)

  formatESResult(res, raw)
}

