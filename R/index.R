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
index <- function (index, type, id, document, routing = NULL, ttl = NULL, refresh = FALSE, timeout = "1m", raw = FALSE) {
  if (missing(index) || missing(type) || missing(document)) {
    stop()
  }
  else {
    url = getOption("res_url")
    path = paste(index, type, sep="/")
    if (!missing(id)) {      
      path = paste(path, id, sep="/")
    }
    args = list(routing = routing, ttl = ttl, refresh = refresh, timeout = timeout)
    url = httr::modify_url(url, "path" = path, "query" = args)
        
    if (!missing(id)) { 
      res = httr::PUT(url, body=document)
    }
    else {
      res = httr::POST(url, body=document)
    }
    httr::stop_for_status(res)
    
    format_res(res, raw)
  }
}