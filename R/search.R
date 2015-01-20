#' search
#'
#' Return documents matching a query, aggregations/facets, highlighted snippets, suggestions, and more.
#' \code{search} executes a search query and retrieves results making the query
#' @param index String A comma-separated list of index names to search; use _all or empty string to perform the operation on all indices
#' @param type String A comma-separated list of document types to search; leave empty to perform the operation on all types
#' @param body Json
#' @param analyzer String The analyzer to use for the query string
#' @param analyze_wildcard Logical Specify whether wildcard and prefix queries should be analyzed (default: false)
#' @param default_operator String The default operator for query string query (AND or OR)
#' @param explain Logical Specify whether to return detailed information about score computation as part of a hit
#' @param fields String, String[], Logical A comma-separated list of fields to return as part of a hit
#' @param from Number Starting offset (default: 0)
#' @param ignore_unavailable String Whether specified concrete indices should be ignored when unavailable (missing or closed)
#' @param allow_no_indices Logical Whether to ignore if a wildcard indices expression resolves into no concrete indices. (This includes _all string or when no indices have been specified)
#' @param expand_wildcards String Whether to expand wildcard expression to concrete indices that are open, closed or both.
#' @param indices_boost String, String[], Logical Comma-separated list of index boosts
#' @param lenient Logical Specify whether format-based query failures (such as providing text to a numeric field) should be ignored
#' @param lowercase_expanded_terms Logical Specify whether query terms should be lowercased
#' @param preference String Specify the node or shard the operation should be performed on (default: random)
#' @param routing String, String[], Boolean A comma-separated list of specific routing values
#' @param scroll Duration Specify how long a consistent view of the index should be maintained for scrolled search
#' @param search_type String Search operation type
#' @param size Number Number of hits to return (default: 10)
#' @param source String, String[], Logical True or false to return the _source field or not, or a list of fields to return
#' @param source_exclude String, String[], Logical A list of fields to exclude from the returned _source field
#' @param source_include String, String[], Logical A list of fields to extract and return from the _source field
#' @param sort String, String[], Logical A list of <field>:<direction> pairs
#' @param timeout Duration Explicit operation timeout
#' @param track_scores Logical Whether to calculate and return scores even if they are not used for sorting
#' @param version Logical Specify whether to return document version as part of a hit
#' @param query_cache Logical Specify if query cache should be used for this request or not, defaults to index level setting
#' @examples
#' search("twitter", id="1")
#' search("twitter", "tweet", "1", 'c("name", "date")')
#'
#' @references
#' \url{http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/search-search.html#search-search}
#'
#' @export
search <- function (client, ...) {
  UseMethod("search", client)
}

#' @rdname search
#' @export
search.elasticsearch <- function (client, index, type, body, from = 0, size = 10, fields = NULL,
                                  source = NULL, default_operator = "OR", explain = FALSE, lowercase_expanded_terms = NULL, preference = "random",
                                  analyzer = NULL, analyze_wildcard = FALSE, ignore_unavailable = NULL, timeout = NULL, source_include = NULL,
                                  allow_no_indices = FALSE, expand_wildcards = "open", search_type = NULL, source_exclude = NULL,
                                  track_scores = NULL, scroll, sort = NULL, query_cache = NULL, raw = FALSE, validate_params = TRUE) {
  # Format path
  path = ""
  if (missing(index) && missing(type)) {
    path = paste("_search", sep="/")
  } else if (missing(index)) {
    path = paste("_all", paste(type, collapse = ","), "_search", sep="/")
  } else if (missing(type)) {
    path = paste(paste(index, collapse = ","), "_search", sep="/")
  } else {
    path = paste(paste(index, collapse = ","),
                 paste(type, collapse = ","), "_search", sep="/")
  }

  args = as.list(match.call())
  args = removeNonURLARgs(args)
  if (validate_params) {
    validateParams(args)
  }
  args = prepareArgs(args)

  url = httr::modify_url(client$url, "path" = path, "query" = args)
  # Send HTTP request
  if (!missing(body)) {
    res = httr::POST(url, body = body, encode = "json")
  } else {
    res = httr::POST(url)
  }
  httr::stop_for_status(res)

  # Return the result
  formatESResult(res, raw)
}
