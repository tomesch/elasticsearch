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
      res = httr::POST(req_url, body=document, content_type_json())
      stop_for_status(res)
      content(res, as="parsed")
    }
    else {
      req_url = paste(req_url, id, sep="/")
      res = httr::PUT(req_url, body=document, content_type_json())
      stop_for_status(res)
      content(res, as="parsed")
    }
  }
}

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
    stop_for_status(res)
   
    if (raw) {
      content(res, as="text")
    }
    else {
      content(res, as="parsed")
    }
  }
}

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