#' @export
search <- function (index, type, query, from = 0, size = 10, fields, source, raw = FALSE) {
  req_url = getOption("relastic_url")
  
  # Format request url
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
  
  # Format request body 
  body = list("from" = jsonlite::unbox(from), "size" = jsonlite::unbox(size))
  
  if (!missing(query)) {
    if (is.list(query)) {
      body = c(body, list("query" = query))
    }
    if (is.character(query) && jsonlite::validate(query)[1]) {
      body = c(body, list("query" = jsonlite::fromJSON(query)))
    }
  }
  
  if (!missing(fields)) {
    body = c(body, list("fields" = c(fields)))
  }
  
  if (!missing(source)) {
    body = c(body, list("source" = source))
  }
  
  # Send HTTP request
  body_json = jsonlite::toJSON(body)
  res = httr::POST(req_url, body = body_json)
  httr::stop_for_status(res)
  
  # Return the result
  if (raw) {
    httr::content(res, as="text")
  }
  else {
    httr::content(res, as="parsed")
  }
}