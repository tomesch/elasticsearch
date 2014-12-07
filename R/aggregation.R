#' Return values extracted from the aggregated document
#'
#' \code{aggregation} returns values extracted from the aggregated document
#'
#' @param index A string representing the index
#' @param type A string representing the type
#' @param aggs A string representing the json aggregation parameter
#'
#' @references
#' \url{http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/search-aggregations-bucket-terms-aggregation.html}
#'
#' @export
aggregation <- function (index, type, aggregations) {
  if (missing(index) || missing(type) || missing(aggregations)) {
    stop()
  }
  else {
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

    # retrieve aggregations from JSON object
    if (!missing(aggregations)) {
      if (is.character(aggregations) && jsonlite::validate(aggregations)[1]) {
        body = c(body, list("aggregations" = jsonlite::fromJSON(aggregations)))
      }
    }
    # Send HTTP request
    body_json = jsonlite::toJSON(body)
    res = httr::POST(req_url, body = body_json)
    print(req_url)
    httr::stop_for_status(res)

    # Retrieve response as JSON
    jsonlite::fromJSON(httr::content(res, as="text"))
  }
}
