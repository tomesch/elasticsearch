#' Execute a search query and retrieve results making the query
#'
#' \code{search} executes a search query and retrieves results making the query
#'
#' @param index A string representing the index.
#' @param type A string representing the type.
#' @param query A string representing the query.
#' @param from A string representing the beginning of the interval we are interested in.
#' @param size A string representing the size.
#' @param fields A string representing the fields.
#' @param source A boolean representing our choice to return the contents of the _source field or not.
#' @param default_operator A string representing the default operator.
#' @param explain A boolean enabling explanation for each hit on how its score was computed.
#' @param analyzer A string that allows to use a document field property as the name of the analyzer that will be used to index the document.
#' @param timeout A string representing the value of the timeout .
#' @param source_include A string representing the source_include; include parameters that you want to display in the returned source.  
#' @param source_exclude A string representing the source_exclude; filter parameters that you do not want to display in the returned source.
#' @param search_type A string representing the type of the search.
#' @param lowercase_expanded_terms A boolean indicating if the wildcard has lower case characters.
#' @param analyze_wildcard A boolean indicating, when true, that an attempt will be made to analyze wildcarded words before searching the term list for matching terms.
#' @param validate A boolean that allows a user to validate a potentially expensive query without executing it. 
#' @param raw A boolean that indicates if the format of the response should be in json or not.
#' @param validate.params A boolean indicating the need to validate the passing parameters or not.
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
                    source = NULL, default_operator = "OR", explain = FALSE,
                    analyzer = NULL, timeout = NULL, source_include = NULL,
                    search_type = "query_then_fetch", source_exclude = NULL,
                    lowercase_expanded_terms = TRUE, analyze_wildcard = FALSE,
                    validate = FALSE, raw = FALSE, validate.params = TRUE) {
  url = getOption("res_url")

  # Format path
  path = ""
  if (missing(index) && missing(type)) {
    path = paste(path, "_search", sep="/")
  } else if (missing(index)) {
    path = paste(path, "_all", paste(type, collapse = ","), "_search", sep="/")
  } else if (missing(type)) {
    path = paste(path, paste(index, collapse = ","), "_search", sep="/")
  } else {
    path = paste(path, paste(index, collapse = ","),
                 paste(type, collapse = ","), "_search", sep="/")
  }

  if (!is.null(fields)) {
    fields = paste(fields, collapse = ",")
  }
  if (!is.null(source_include)) {
    source_include = paste(source_include, collapse = ",")
  }
  if (!is.null(source_exclude)) {
    source_exclude = paste(source_exclude, collapse = ",")
  }

  args = list("_source_exclude" = source_exclude,
              "_source_include" = source_include, fields = fields,
              "_source" = source, default_operator = default_operator,
              explain = explain, analyzer = analyzer, timeout = timeout,
              search_type = search_type,
              lowercase_expanded_terms = lowercase_expanded_terms,
              analyze_wildcard = analyze_wildcard, from = from, size = size)

  if (validate.params) {
    validateArgs(args)
  }

  args = prepareArgs(args)

  if (validate) {
    print(path)
  }

  url = httr::modify_url(url, "path" = path, "query" = args)

  # Send HTTP request
  if (!missing(query)) {
    res = httr::POST(url, body = query, encode = "json")
  } else {
    res = httr::POST(url)
  }
  httr::stop_for_status(res)

  # Return the result
  formatESResult(res, raw)
}
