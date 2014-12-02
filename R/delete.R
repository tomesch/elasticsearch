#' Delete a document from an index based on its id
#'
#' \code{delete} deletes a document from an index based on its id. 
#'
#' @param index A string representing the index under wi
#' @param type A string representing the index under wi
#' @param id A string representing the index under wi
#' 
#' @examples
#' delete("twitter", "tweet", 1)
#' 
#' @references
#' \url{http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/docs-delete.html}
#' 
#' @export
delete <- function (index, type, id) {
  if (missing(index) || missing(type) || missing(id)) {
    stop()
  }
  else {
    base_url = getOption("relastic_url")
    req_url = paste(base_url, index, type, id, sep="/")
    
    res <- DELETE(req_url)
    stop_for_status(res)
    content(res, as="parsed")
  }
}