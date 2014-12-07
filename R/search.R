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
search <- function (index, type, query, from = 0, size = 10, fields = NULL,
                    source = NULL, default_operator="OR", explain=FALSE,
                    analyzer = NULL, timeout = NULL,
                    search_type = "query_then_fetch",
                    lowercase_expanded_terms = TRUE, analyze_wildcard = FALSE,
                    raw = FALSE, validate = TRUE) {
  url = getOption("res_url")

  # Format path
  if (missing(index) && missing(type)) {
    path = paste(path, "_search", sep="/")
  }
  else if (missing(index)) {
    path = paste(path, "_all", paste(type, collapse = ","), "_search", sep="/")
  }
  else if (missing(type)) {
    path = paste(path, paste(index, collapse = ","), "_search", sep="/")
  }
  else {
    path = paste(path, paste(index, collapse = ","),
                 paste(type, collapse = ","), "_search", sep="/")
  }

  if (!is.null(fields)) {
    fields = paste(fields, collapse = ",")
  }

  args = list(fields = fields, "_source" = source,
              default_operator = default_operator, explain = explain,
              analyzer = analyzer, timeout = timeout, search_type = search_type,
              lowercase_expanded_terms = lowercase_expanded_terms,
              analyze_wildcard = analyze_wildcard, from = from, size = size)

  if (validate) validate_args(args)

  url = httr::modify_url(url, "path" = path, "query" = args)

  # Send HTTP request
  res = httr::POST(url, body = query, encode = "json")
  httr::stop_for_status(res)

  # Return the result
  format_res(res, raw)
}
