#' Execute a search query and retrieve results macking the query
#'  
#' \code{search} executes a search query and retrieves∆ results macking the query
#'
#' @param index A string representing the index under wi
#' @param type A string representing the index under wi
#' @param query A string representing the index under wi
#' @param from A string representing the index under wi
#' @param size A string representing the index under wi
#' @param fields A string representing the index under wi
#' 
#' @examples
#' search("twitter", id="1")
#' search("twitter", "tweet", "1", 'c("name", "date")')
#' 
#' @references
#' \url{http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/search-search.html#search-search}
#' 
#' @export
search <- function (index, type, query, from = 0, size = 10, fields, source, default_operator="OR", explain=FALSE, raw = FALSE) {
  req_url = getOption("relastic_url")
  
  # Format base request url
  if (missing(index) && missing(type)) {
    req_url = paste(req_url, "_search", sep="/")
  }
  else if (missing(index)) {
    req_url = paste(req_url, "_all", paste(type, collapse = ","), "_search", sep="/")
  }
  else if (missing(type)) {
    req_url = paste(req_url, paste(index, collapse = ","), "_search", sep="/")
  }
  else {
    req_url = paste(req_url, paste(index, collapse = ","), paste(type, collapse = ","), "_search", sep="/")
  }
  
  # Add uri parameters
  req_url = paste0(req_url, "?default_operator=", default_operator)
  req_url = paste0(req_url, "&explain=", explain)
  
  # Format request body 
  body = list("from" = jsonlite::unbox(from), "size" = jsonlite::unbox(size))
  
  if (!missing(query)) {
    if (is.character(query) && jsonlite::validate(query)[1]) {
      body = c(body, list("query" = jsonlite::fromJSON(query)))
    }
  }
  
  if (!missing(fields)) {
    body = c(body, list("fields" = c(fields)))
  }
    
  if (!missing(source)) {
    if (is.logical(source) || is.character(source)) {
      body = c(body, list("_source" = jsonlite::unbox(source)))
    }
    else if (is.vector(source)) {
      body = c(body, list("_source" = source))
    }
  }
  
  # Send HTTP request
  body_json = jsonlite::toJSON(body)
  res = httr::POST(req_url, body = body_json)
  print(req_url)
  httr::stop_for_status(res)
  
  # Return the result
  if (raw) {
    httr::content(res, as="text")
  }
  else {
    jsonlite::fromJSON(httr::content(res, as="text"))
  }
}