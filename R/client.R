#' ElasticsearchClient
#'
#' Create an Elasticsearch client instance
#' @export
ElasticsearchClient <- function(host = "http://localhost:9200") {
  url = httr::parse_url(host)

  res <- httr::GET(url)
  httr::stop_for_status(res)

  res = formatESResult(res, FALSE)
  if (res$status != 200) {
    stop()
  }

  structure(list("url" = url), class = "elasticsearch")
}
