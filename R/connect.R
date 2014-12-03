#' @export
connect <- function (url = "http://127.0.0.1:9200/", raw = FALSE) {
  url = httr::parse_url(url)
  
  res <- httr::GET(url)
  httr::stop_for_status(res)
  configure(url)
  
  format_res(res, raw)
}

configure <- function (url) {
  options(res_url = url)
}