#' Add or update a document in a specific index
#'
#' \code{index} adds or updates a document in a specific index. 
#'
#' @param index A string representing the index under wi
#' @param type A string representing the index under wi
#' @param id A string representing the index under wi
#' @param document A string representing the index under wi
#' 
#' @examples
#' index("twitter", "tweet", 1, '{"test_index": "text_index"}')
#' index("twitter", "tweet", document='{"test_index": "text_index"}')
#' 
#' @references
#' \url{http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/docs-index_.html}
#'
#' @export
index <- function (index, type, id=NULL, document) {
  if (missing(index) || missing(type) || missing(document)) {
    stop()
  }
  else {
    base_url = getOption("relastic_url")
    req_url = paste(base_url, index, type, sep="/")
    if (is.null(id)) {      
      res = httr::POST(req_url, body=document)
      httr::stop_for_status(res)
      httr::content(res, as="parsed")
    }
    else {
      req_url = paste(req_url, id, sep="/")
      res = httr::PUT(req_url, body=document)
      httr::stop_for_status(res)
      httr::content(res, as="parsed")
    }
  }
}