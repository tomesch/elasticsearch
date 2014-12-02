#' @export
search <- function (index = "_all", type = "_all", query = NULL, from = NULL, size = NULL, fields = NULL, raw = FALSE) {
  if (!is.null(query)) {
    if (is.list(query)) {
      body = list("query" = query, "from" = from, "size" = size, "fields" = fields)
    }
    else if (is.character(query) && jsonlite::validate(query)[1]) {
      query_parsed = jsonlite::fromJSON(query)
      #body = list("query" = query_parsed, "from" = from, "size" = size, "fields" = fields)
      body = list("query" = query_parsed)
    }
  }
  else{
    body = list("from" = from, "size" = size, "fields" = fields)
  }
  
  body_json = jsonlite::toJSON(body, auto_unbox = TRUE)
  
  base_url = getOption("relastic_url")
  req_url = paste(base_url, index, type, "_search", sep="/")
  
  res = httr::GET(req_url, body=body_json)
  httr::stop_for_status(res)

  if (raw) {
    httr::content(res, as="text")
  }
  else {
    httr::content(res, as="parsed")
  }
}