#' @export
ElasticSearchClient <- function(host = "http://localhost:9200") {
  url = httr::parse_url(host)

  res <- httr::GET(url)
  httr::stop_for_status(res)

  structure(list("url" = url), class = "elasticsearch")
}
