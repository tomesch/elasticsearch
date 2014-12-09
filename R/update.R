#' Update a document with a script
#'
#' \code{update} updates a document based on a script provided.
#'
#' @param index A string representing the index under wi
#' @param type A string representing the index under wi
#' @param id A string representing the index under wi
#' @param script A string representing the index under wi
#'
#' @examples
#' update("twitter", "tweet", 1)
#'
#' @references
#' \url{http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/docs-update.html}
#'
#' @export
update <- function (index, type, id, body, routing = NULL, parent = NULL,
                    timeout = "1m", refresh = FALSE, fields = NULL,
                    version = NULL, validate.params = TRUE) {
  if (missing(index) || missing(type) || missing(id) || missing(script)) {
    stop()
  } else {
    url = getOption("url")

    path = paste(index, type, id, "_update", sep="/")

    if (!is.null(fields)) {
      fields = paste(fields, collapse = ",")
    }
    args = list(routing = routing, parent = parent, timeout = timeout,
                refresh = refresh, fields = fields, version = version)

    if (validate.params) {
      validateArgs(args)
    }

    args = prepareArgs(args)

    url = httr::modify_url(url, "path" = path, "query" = args)

    res <- httr::POST(url, body=script, content_type_json())
    stop_for_status(res)

    formatESResult(res, raw)
  }
}
