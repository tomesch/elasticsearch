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
get <- function (index, type="_all", id, fields=NULL, raw=FALSE) {
  if (exists("index") && exists("id")) {
    base_url = getOption("relastic_url")
    req_url = paste(base_url, index, type, id, sep="/")
    
    if (is.vector(fields)) {
      fields_str = paste(fields, collapse=",")
      req_url = paste(req_url, "?fields=", fields_str, sep="")
    }
    
    res <- httr::GET(req_url)
    httr::stop_for_status(res)
    
    if (raw) {
      httr::content(res, as="text")
    }
    else {
      httr::content(res, as="parsed")
    }
  }
}