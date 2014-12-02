#' @export
connect <- function (host="http://127.0.0.1", port=9200) {
  base_url = paste(host, port, sep=":")
  res <- httr::GET(base_url)
  httr::stop_for_status(res)
  print(httr::content(res, as="parsed"))
  
  configure(base_url)
}

configure <- function (url) {
  options(relastic_url = url)
}