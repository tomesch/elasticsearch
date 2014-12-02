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
update <- function (index, type, id, script) {
  if (missing(index) || missing(type) || missing(id) || missing(script)) {
    stop()
  }
  else {
    base_url = getOption("relastic_url")
    req_url = paste(base_url, index, type, id, "_update", sep="/")
    
    res <- httr::POST(req_url, body=script, content_type_json())
    stop_for_status(res)
    content(res, as="parsed")
  }
}